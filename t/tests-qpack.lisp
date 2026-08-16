(in-package #:http-message-kit/test)

(describe "QPACK header blocks"
  (it "round trips static and literal fields"
    (let* ((headers (list (make-http-pseudo-header ":method" "GET")
                          (make-http-pseudo-header ":scheme" "https")
                          (make-http-pseudo-header ":path" "/")
                          (make-http-header "x-test" "value")))
           (context (make-qpack-context :huffman nil))
           (wire (qpack-encode-header-block headers
                                             :context context
                                             :huffman nil)))
      (multiple-value-bind (decoded used required base)
          (qpack-decode-header-block wire :context context)
        (expect decoded :to-equalp headers)
        (expect used :to-be (length wire))
        (expect required :to-be 0)
        (expect base :to-be 0))))

  (it "applies encoder-stream inserts before decoding dynamic references"
    (let* ((encoder (make-qpack-context :max-table-capacity 256
                                         :table-capacity 256
                                         :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 256
                                         :table-capacity 256
                                         :huffman nil))
           (insert-wire (qpack-encode-insert-with-literal-name
                         encoder "x-test" "one" :huffman nil)))
      (multiple-value-bind (events used)
          (qpack-decode-encoder-stream decoder insert-wire)
        (expect used :to-be (length insert-wire))
        (expect (first events) :to-equalp
                '(:insert "x-test" "one" 0 :huffman nil)))
      (let* ((headers (list (make-http-header "x-test" "one")))
             (wire (qpack-encode-header-block headers
                                               :context encoder
                                               :use-dynamic-p t
                                               :huffman nil)))
        (multiple-value-bind (decoded used required base)
            (qpack-decode-header-block wire :context decoder)
          (expect decoded :to-equalp headers)
          (expect used :to-be (length wire))
          (expect required :to-be 1)
          (expect base :to-be 1)))))

  (it "signals blocked sections until the required insert arrives"
    (let* ((encoder (make-qpack-context :max-table-capacity 256
                                         :table-capacity 256
                                         :huffman nil))
           (empty-decoder (make-qpack-context :max-table-capacity 256
                                               :table-capacity 256
                                               :huffman nil)))
      (qpack-encode-insert-with-literal-name
       encoder "x-blocked" "yes" :huffman nil)
      (let ((wire (qpack-encode-header-block
                   (list (make-http-header "x-blocked" "yes"))
                   :context encoder
                   :use-dynamic-p t
                   :huffman nil)))
        (signals http-qpack-blocked
          (qpack-decode-header-block wire :context empty-decoder)))))

  (it "round trips decoder-stream control instructions"
    (let ((context (make-qpack-context :huffman nil)))
      (dolist (spec (list (list (qpack-encode-section-ack 7)
                                '((:section-ack 7)))
                          (list (qpack-encode-stream-cancellation 9)
                                '((:stream-cancellation 9)))))
        (multiple-value-bind (events used)
            (qpack-decode-decoder-stream context (first spec))
          (expect events :to-equalp (second spec))
          (expect used :to-be (length (first spec)))))
      (signals http-protocol-error
        (qpack-encode-insert-count-increment 0)))))

(describe "compression input boundaries"
  (it "does not decode beyond an HPACK block end"
    (let ((wire (octets 0 1 120 1 121)))
      (signals http-protocol-error
        (hpack-decode-header-block wire :end 4))
      (multiple-value-bind (headers used)
          (hpack-decode-header-block wire :end 5)
        (expect headers :to-equalp
                (list (make-http-header "x" "y")))
        (expect used :to-be 5))))

  (it "does not decode beyond a QPACK block end"
    (let ((wire (octets 0 0 #x21 120 1 121)))
      (signals http-incomplete-message
        (qpack-decode-header-block wire :end 5))
      (multiple-value-bind (headers used required base)
          (qpack-decode-header-block wire :end 6)
        (expect headers :to-equalp
                (list (make-http-header "x" "y")))
        (expect used :to-be 6)
        (expect required :to-be 0)
        (expect base :to-be 0)))))
