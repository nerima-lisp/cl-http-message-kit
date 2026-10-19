(in-package #:http-message-kit/test)

(describe "HPACK codec delegation"
  (it "round trips headers through the codec and shares dynamic table state"
    (let* ((headers (list (make-http-header "x-test" "value")))
           (encoder (make-hpack-context :huffman nil))
           (decoder (make-hpack-context :huffman nil))
           (wire (hpack-encode-header-block headers :context encoder)))
      (multiple-value-bind (decoded used)
          (hpack-decode-header-block wire :context decoder)
        (expect decoded :to-equalp headers)
        (expect used :to-be (length wire)))
      (expect (hpack-context-dynamic-entry-count encoder) :to-be 1)
      (expect (hpack-context-table-size encoder)
              :to-be (hpack-context-table-size decoder))))

  (it "encodes a static-only block without changing dynamic table state"
    (let* ((context (make-hpack-context :huffman nil))
           (wire
             (hpack-encode-header-block
              (list (make-http-header "x-static" "value"))
              :context context
              :static-only-p t)))
      (expect (> (length wire) 0) :to-equalp t)
      (expect (hpack-context-dynamic-entry-count context) :to-be 0)))

  (it "uses a sensitive predicate for dynamic and static-only encoding"
    (let* ((header (list (make-http-header "x-private" "secret")))
           (vector-arguments '())
           (predicate
             (lambda (name value)
               (push (and (typep name '(vector (unsigned-byte 8)))
                          (typep value '(vector (unsigned-byte 8))))
                     vector-arguments)
               (and (equalp name (octets 120 45 112 114 105 118 97 116 101))
                    (equalp value (octets 115 101 99 114 101 116)))))
           (dynamic-wire
             (hpack-encode-header-block
              header :huffman nil :indexing :incremental
              :sensitive-predicate predicate))
           (static-wire
             (hpack-encode-header-block
              header :huffman nil :static-only-p t
              :sensitive-predicate predicate)))
      (expect (logand (aref dynamic-wire 0) #xf0) :to-be #x10)
      (expect (logand (aref static-wire 0) #xf0) :to-be #x10)
      (expect (length vector-arguments) :to-be 2)
      (expect (every #'identity vector-arguments) :to-be t)))

  (it "clamps per-call decode limits to the configured decoder limits"
    (let* ((encoder (make-hpack-context :huffman nil))
           (wire
             (hpack-encode-header-block
              (list (make-http-header "x-one" "1")
                    (make-http-header "x-two" "2"))
              :context encoder))
           (decoder (make-hpack-context :max-field-count 1)))
      (signals http-protocol-error
        (hpack-decode-header-block wire :context decoder :max-fields 10))))

  (it "clamps the per-call byte limit and keeps configured name validation"
    (let ((wire (octets 0 5 120 45 111 110 101 1 49))
          (uppercase-wire (octets 0 1 88 1 118))
          (size-limited (make-hpack-context :max-header-list-size 37))
          (validated (make-hpack-context :validate-header-names t)))
      (signals http-protocol-error
        (hpack-decode-header-block
         wire :context size-limited :max-header-bytes 100))
      (signals http-protocol-error
        (hpack-decode-header-block
         uppercase-wire :context validated :validate-header-names nil))
      (signals http-protocol-error
        (hpack-decode-header-block
         wire :context validated :validate-header-names :enabled))))

  (it "makes header-name validation optional when it is not configured"
    (let ((wire (octets 0 1 88 1 118)))
      (expect (hpack-decode-header-block wire)
              :to-equalp (list (make-http-header "X" "v")))
      (signals http-protocol-error
        (hpack-decode-header-block wire :validate-header-names t))))

  (it "bounds the prefixed integer codec to the HPACK integer domain"
    (expect (hpack-encode-integer 133 5) :to-equalp (octets #x1f #x66))
    (multiple-value-bind (value used)
        (hpack-decode-integer (octets #x1f #x66) 0 5)
      (expect value :to-be 133)
      (expect used :to-be 2))))
