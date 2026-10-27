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
                                         :max-blocked-streams 1
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
                                               :stream-id 4
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
                                         :max-blocked-streams 1
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
                   :stream-id 6
                   :huffman nil)))
        (signals http-qpack-blocked
          (qpack-decode-header-block wire :context empty-decoder)))))

  (it "round trips decoder-stream control instructions"
    (let* ((context (make-qpack-context :max-table-capacity 128
                                        :table-capacity 128
                                        :max-blocked-streams 1
                                        :huffman nil))
           (headers (list (make-http-header "x" "v"))))
      (qpack-encode-insert-with-literal-name context "x" "v" :huffman nil)
      (let ((wire (qpack-encode-header-block
                   headers :context context :use-dynamic-p t
                   :stream-id 7 :huffman nil)))
        (multiple-value-bind (decoded used required)
            (qpack-decode-header-block wire :context context :stream-id 7)
          (declare (ignore used))
          (expect decoded :to-equalp headers)
          (expect required :to-be 1)))
      (dolist (spec (list (list (qpack-encode-section-ack 7)
                                (list (list :section-ack 7)))
                          (list (qpack-encode-stream-cancellation 9)
                                (list (list :stream-cancellation 9)))))
        (multiple-value-bind (events used)
            (qpack-decode-decoder-stream context (first spec))
          (expect events :to-equalp (second spec))
          (expect used :to-be (length (first spec)))))
      (signals http-protocol-error
        (qpack-encode-insert-count-increment 0)))))
(describe "QPACK wire fixtures and input boundaries"
  (it "decodes the RFC 9204 Appendix B.1 static field section"
    (let ((wire (octets 0 0 #x51 #x0b
                       47 105 110 100 101 120 46 104 116 109 108)))
      (multiple-value-bind (headers used required base)
          (qpack-decode-header-block
           wire :context (make-qpack-context :huffman nil))
        (expect headers :to-equalp
                (list (make-http-pseudo-header ":path" "/index.html")))
        (expect used :to-be (length wire))
        (expect required :to-be 0)
        (expect base :to-be 0))))

  (it "replays the RFC 9204 Appendix B.2-B.5 encoder and decoder streams"
    (let* ((decoder (make-qpack-context :max-table-capacity 220
                                       :table-capacity 0
                                       :max-blocked-streams 1
                                       :huffman nil))
           (feedback (make-qpack-context :max-table-capacity 220
                                         :table-capacity 0
                                         :max-blocked-streams 1
                                         :huffman nil))
           (b2-encoder-stream
             (octets #x3f #xbd #x01
                     #xc0 #x0f
                     119 119 119 46 101 120 97 109 112 108 101 46 99 111 109
                     #xc1 #x0c
                     47 115 97 109 112 108 101 47 112 97 116 104))
           (b2-field-section (octets #x03 #x81 #x10 #x11))
           (b3-encoder-stream
             (octets #x4a
                     99 117 115 116 111 109 45 107 101 121
                     #x0c
                     99 117 115 116 111 109 45 118 97 108 117 101))
           (b4-field-section (octets #x05 #x00 #x80 #xc1 #x81)))
      (qpack-encode-set-capacity feedback 220)
      (multiple-value-bind (events used)
          (qpack-decode-encoder-stream decoder b2-encoder-stream)
        (declare (ignore events))
        (expect used :to-be (length b2-encoder-stream)))
      (expect (qpack-context-table-capacity decoder) :to-be 220)
      (expect (qpack-context-dynamic-entry-count decoder) :to-be 2)
      (expect (qpack-context-table-size decoder) :to-be 106)
      (expect (qpack-context-insert-count decoder) :to-be 2)
      (multiple-value-bind (headers used required base)
          (qpack-decode-header-block b2-field-section :context decoder)
        (expect headers :to-equalp
                (list (make-http-pseudo-header ":authority" "www.example.com")
                      (make-http-pseudo-header ":path" "/sample/path")))
        (expect used :to-be (length b2-field-section))
        (expect required :to-be 2)
        (expect base :to-be 0))
      (qpack-encode-insert-with-name-reference
       feedback 0 "www.example.com" :huffman nil)
      (qpack-encode-insert-with-name-reference
       feedback 1 "/sample/path" :huffman nil)
      (qpack-encode-header-block
       (list (make-http-pseudo-header ":authority" "www.example.com")
             (make-http-pseudo-header ":path" "/sample/path"))
       :context feedback
       :use-dynamic-p t
       :stream-id 4
       :huffman nil)
      (multiple-value-bind (events used)
          (qpack-decode-decoder-stream feedback (octets #x84))
        (expect events :to-equalp (list (list :section-ack 4)))
        (expect used :to-be 1))
      (qpack-encode-insert-with-literal-name
       feedback "custom-key" "custom-value" :huffman nil)
      (multiple-value-bind (events used)
          (qpack-decode-encoder-stream decoder b3-encoder-stream)
        (expect events :to-equalp
                (list (list :insert "custom-key" "custom-value" 2
                            :huffman nil)))
        (expect used :to-be (length b3-encoder-stream)))
      (expect (qpack-context-dynamic-entry-count decoder) :to-be 3)
      (expect (qpack-context-table-size decoder) :to-be 160)
      (expect (qpack-context-insert-count decoder) :to-be 3)
      (multiple-value-bind (events used)
          (qpack-decode-decoder-stream feedback (octets #x01))
        (expect events :to-equalp
                (list (list :insert-count-increment 1)))
        (expect used :to-be 1))
      ;; The API reports a blocked section but has no transport queue to cancel later.
      (signals http-qpack-blocked
        (qpack-decode-header-block b4-field-section :context decoder))
      (multiple-value-bind (events used)
          (qpack-decode-encoder-stream decoder (octets #x02))
        (declare (ignore events))
        (expect used :to-be 1))
      (expect (qpack-context-dynamic-entry-count decoder) :to-be 4)
      (expect (qpack-context-table-size decoder) :to-be 217)
      (expect (qpack-context-insert-count decoder) :to-be 4)
      (qpack-encode-insert-with-name-reference
       feedback 0 "www.example.com" :huffman nil)
      (qpack-encode-header-block
       (list (make-http-pseudo-header ":authority" "www.example.com")
             (make-http-pseudo-header ":path" "/")
             (make-http-header "custom-key" "custom-value"))
       :context feedback
       :use-dynamic-p t
       :stream-id 8
       :huffman nil)
      (multiple-value-bind (events used)
          (qpack-decode-decoder-stream feedback (octets #x48))
        (expect events :to-equalp
                (list (list :stream-cancellation 8)))
        (expect used :to-be 1))
      (expect (qpack-context-table-size decoder) :to-be 217)
      (multiple-value-bind (events used)
          (qpack-decode-encoder-stream
           decoder
           (octets #x81 #x0d
                   99 117 115 116 111 109 45 118 97 108 117 101 50))
        (expect events :to-equalp
                (list (list :insert "custom-key" "custom-value2" 4
                            :huffman nil)))
        (expect used :to-be 15))
      (expect (qpack-context-dynamic-entry-count decoder) :to-be 4)
      (expect (qpack-context-table-size decoder) :to-be 215)
      (expect (qpack-context-insert-count decoder) :to-be 5)
      (signals http-protocol-error
        (qpack-decode-header-block
         (octets #x02 #x00 #x80) :context decoder))))

  (it "leaves the N bit clear on ordinary literal field lines"
    (let* ((context (make-qpack-context :huffman nil))
           (wire (qpack-encode-header-block
                  (list (make-http-header "x-test" "v"))
                  :context context
                  :huffman nil)))
      (expect (aref wire 2) :to-be #x26)
      (expect (logand (aref wire 2) #x10) :to-be 0)))
  (it "sets the N bit on sensitive static-name references"
    (let* ((context (make-qpack-context :huffman nil))
           (wire (qpack-encode-header-block
                  (list (make-http-header "cookie" "session"))
                  :context context
                  :huffman nil)))
      (expect (aref wire 2) :to-be #x75)))

  (it "rejects a post-base reference outside the required insert count"
    (let* ((encoder (make-qpack-context :max-table-capacity 128
                                       :table-capacity 128
                                       :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 128
                                        :huffman nil)))
      (dolist (name (list "x-one" "x-two" "x-three"))
        (qpack-decode-encoder-stream
         decoder
         (qpack-encode-insert-with-literal-name
          encoder name "v" :huffman nil)))
      (signals http-protocol-error
        (qpack-decode-header-block
         (octets #x02 #x01 #x10) :context decoder))))

  (it "rejects raw uppercase names in encoder-stream literal inserts"
    (let ((context (make-qpack-context :max-table-capacity 64
                                       :table-capacity 64
                                       :huffman nil)))
      (signals http-protocol-error
        (qpack-decode-encoder-stream
         context (octets #x41 #x58 #x01 #x76)))
      (expect (qpack-context-insert-count context) :to-be 0)
      (expect (qpack-context-dynamic-entry-count context) :to-be 0)))

  (it "encodes and decodes the 62-bit stream ID maximum"
    (let* ((stream-id (1- (ash 1 62)))
           (context (make-qpack-context :max-table-capacity 128
                                        :table-capacity 128
                                        :max-blocked-streams 1
                                        :huffman nil))
           (headers (list (make-http-header "x" "v"))))
      (qpack-encode-insert-with-literal-name context "x" "v" :huffman nil)
      (let ((wire (qpack-encode-header-block
                   headers :context context :use-dynamic-p t
                   :stream-id stream-id :huffman nil)))
        (multiple-value-bind (decoded used required)
            (qpack-decode-header-block wire :context context :stream-id stream-id)
          (declare (ignore used))
          (expect decoded :to-equalp headers)
          (expect required :to-be 1)))
      (multiple-value-bind (events used)
          (qpack-decode-decoder-stream
           context (qpack-encode-section-ack stream-id))
        (expect events :to-equalp (list (list :section-ack stream-id)))
        (expect used :to-be
                (length (qpack-encode-section-ack stream-id))))
      (multiple-value-bind (events used)
          (qpack-decode-decoder-stream
           context (qpack-encode-stream-cancellation stream-id))
        (expect events :to-equalp
                (list (list :stream-cancellation stream-id)))
        (expect used :to-be
                (length (qpack-encode-stream-cancellation stream-id))))))

  (it "rejects malformed and out-of-range QPACK integers"
    (let ((context (make-qpack-context :huffman nil)))
      (signals http-protocol-error
        (qpack-decode-decoder-stream context (octets #xff #x80)))
      (signals http-protocol-error
        (qpack-decode-decoder-stream
         context
         (octets #xff #x81 #xff #xff #xff #xff #xff #xff #xff #x3f)))
      (signals http-protocol-error
        (qpack-encode-section-ack (ash 1 62)))
      (signals http-protocol-error
        (qpack-encode-stream-cancellation (ash 1 62))))))

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
        (expect base :to-be 0))))

  (it "bounds decoded QPACK strings by the field-section byte limit"
    (let* ((context (make-qpack-context :huffman t))
           (wire (qpack-encode-header-block
                  (list (make-http-header "x" "v"))
                  :context context
                  :huffman t)))
      (signals http-size-limit-exceeded
        (qpack-decode-header-block wire :max-header-bytes 33))))

  (it "bounds encoder-stream literals by the dynamic-table capacity"
    (let ((context (make-qpack-context :max-table-capacity 40
                                       :table-capacity 40
                                       :huffman nil)))
      (signals http-size-limit-exceeded
        (qpack-decode-encoder-stream
         context
         (octets #x41 120 #x08 49 50 51 52 53 54 55 56)))
      (expect (qpack-context-insert-count context) :to-be 0)
      (expect (qpack-context-dynamic-entry-count context) :to-be 0))))

(describe "QPACK static table and blocked-stream behavior"
  (it "matches RFC 9204 static-table entry 58"
    (multiple-value-bind (headers used required base)
        (qpack-decode-header-block (octets 0 0 #xfa))
      (expect headers :to-equalp
              (list
               (make-http-header
                "strict-transport-security"
                "max-age=31536000; includesubdomains; preload")))
      (expect used :to-be 3)
      (expect required :to-be 0)
      (expect base :to-be 0)))

  (it "preserves pseudo-field order and leaves semantic validation to HTTP/3"
    (let* ((headers
             (list (make-http-header "x-regular" "v")
                   (make-http-pseudo-header ":method" "GET")
                   (make-http-pseudo-header ":scheme" "https")
                   (make-http-pseudo-header ":path" "/")
                   (make-http-pseudo-header ":authority" "example.test")))
           (wire (qpack-encode-header-block headers :huffman nil)))
      (multiple-value-bind (decoded used required base)
          (qpack-decode-header-block wire)
        (expect decoded :to-equalp headers)
        (expect used :to-be (length wire))
        (expect required :to-be 0)
        (expect base :to-be 0)
        (signals http-invalid-header
          (http3-validate-header-section decoded :request-p t)))))

  (it "resumes from original field-section bytes after caller mutation"
    (let* ((wire (octets 2 0 #x80))
           (decoder-table
             (http-message-kit::make-qpack-dynamic-table :max-capacity 128))
           (context
             (http-message-kit::make-http3-qpack-decoder-context
              decoder-table :blocked-stream-limit 1))
           (blocked
             (http-message-kit::decode-http3-qpack-field-section
              wire context 3)))
      (expect (http-message-kit::http3-qpack-blocked-stream-p blocked)
              :to-be t)
      (setf (aref wire 2) #x81)
      (http-message-kit::qpack-dynamic-table-insert
       decoder-table "x-original" "v")
      (expect (http-message-kit::%http3-qpack-detach-ready-streams context)
              :to-equalp (list blocked))
      (expect (http-message-kit::resume-http3-qpack-blocked-stream blocked)
              :to-equalp (list (cons "x-original" "v")))))

  (it "rejects oversized blocked sections before registering a stream"
    (let* ((table
             (http-message-kit::make-qpack-dynamic-table :max-capacity 128))
           (context
             (http-message-kit::make-http3-qpack-decoder-context
              table :blocked-stream-limit 1))
           (wire (make-array 34 :element-type '(unsigned-byte 8)
                             :initial-element 0)))
      (signals http-size-limit-exceeded
        (http-message-kit::decode-http3-qpack-field-section
         wire context 3 :max-header-bytes 1 :max-fields 1))
      (expect
       (hash-table-count
        (http-message-kit::http3-qpack-decoder-context-blocked-streams context))
       :to-be 0))))
(describe "QPACK HTTP/3 transport adapter"
  (it "resumes a field section after encoder-stream inserts on the shared context"
    (let* ((encoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (capacity-wire (qpack-encode-set-capacity encoder 128))
           (insert-wire (qpack-encode-insert-with-literal-name
                         encoder "x-field" "value" :huffman nil))
           (instructions (concatenate '(vector (unsigned-byte 8))
                                      capacity-wire insert-wire))
           (wire (octets 2 0 #x80))
           (blocked (qpack-decode-http3-field-section
                     wire adapter 3 :role :trailers)))
      (expect (qpack-blocked-stream-p blocked) :to-be t)
      (expect (qpack-take-ready-streams adapter) :to-equal nil)
      (multiple-value-bind (events consumed)
          (qpack-decode-encoder-stream decoder instructions)
        (expect consumed :to-be (length instructions))
        (expect (mapcar #'first events) :to-equal '(:set-capacity :insert)))
      (expect (qpack-take-ready-streams adapter) :to-equal (list blocked))
      (expect (qpack-resume-blocked-stream blocked)
              :to-equalp (list (cons "x-field" "value")))
      (expect (hash-table-count
               (http-message-kit::http3-qpack-decoder-context-latest-streams adapter))
              :to-be 0)
      (signals http-protocol-error
        (qpack-resume-blocked-stream blocked))))

  (it "validates request, response, and trailer sections before returning fields"
    (let* ((decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (request-wire
             (octets 0 0 #xd1 #xd7 #xc1 #x50 #x0c
                     101 120 97 109 112 108 101 46 116 101 115 116)))
      (expect (qpack-decode-http3-field-section
               request-wire adapter 0 :role :request)
              :to-equalp
              (list (cons ":method" "GET")
                    (cons ":scheme" "https")
                    (cons ":path" "/")
                    (cons ":authority" "example.test")))
      (expect (qpack-decode-http3-field-section
               (octets 0 0 #xd9) adapter 4 :role :response)
              :to-equalp (list (cons ":status" "200")))
      (expect (qpack-decode-http3-field-section
               (octets 0 0) adapter 8 :role :trailers)
              :to-equal nil)
      (signals http-invalid-header
        (qpack-decode-http3-field-section
         (octets 0 0 #xd1) adapter 12 :role :request))))

  (it "checks request-section semantics again when resuming a blocked section"
    (let* ((encoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (capacity-wire (qpack-encode-set-capacity encoder 128))
           (insert-wire (qpack-encode-insert-with-literal-name
                         encoder "x-field" "value" :huffman nil))
           (instructions (concatenate '(vector (unsigned-byte 8))
                                      capacity-wire insert-wire))
           (blocked (qpack-decode-http3-field-section
                     (octets 2 0 #x80) adapter 3 :role :request)))
      (expect (qpack-blocked-stream-p blocked) :to-be t)
      (qpack-decode-encoder-stream decoder instructions)
      (expect (qpack-take-ready-streams adapter) :to-equal (list blocked))
      (signals http-invalid-header
        (qpack-resume-blocked-stream blocked))
      (expect (hash-table-count
               (http-message-kit::http3-qpack-decoder-context-latest-streams adapter))
              :to-be 0)))

  (it "returns RFC stream-cancellation bytes and frees blocked-stream capacity"
    (let* ((decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (peer (make-qpack-context :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (blocked (qpack-decode-http3-field-section
                     (octets 2 0 #x80) adapter 3 :role :trailers))
           (serialize-calls 0))
      (expect (qpack-blocked-stream-p blocked) :to-be t)
      (multiple-value-bind (released-p instruction)
          (qpack-cancel-blocked-stream
           adapter 3
           :serialize (lambda (thunk)
                        (incf serialize-calls)
                        (funcall thunk)))
        (expect released-p :to-be t)
        (expect instruction :to-equalp (octets #x43))
        (multiple-value-bind (events consumed)
            (qpack-decode-decoder-stream peer instruction)
          (expect events :to-equalp (list (list :stream-cancellation 3)))
          (expect consumed :to-be (length instruction))))
      (expect serialize-calls :to-be 1)
      (expect (hash-table-count
               (http-message-kit::http3-qpack-decoder-context-blocked-streams adapter))
              :to-be 0)
      (expect (hash-table-count
               (http-message-kit::http3-qpack-decoder-context-latest-streams adapter))
              :to-be 0)
      (expect (http-message-kit::http3-qpack-blocked-stream-resume-function blocked)
              :to-be nil)
      (expect (http-message-kit::http3-qpack-blocked-stream-serialize blocked) :to-be nil)
      (expect (http-message-kit::http3-qpack-blocked-stream-context blocked) :to-be nil)
      (let ((next (qpack-decode-http3-field-section
                   (octets 2 0 #x80) adapter 7 :role :trailers)))
        (expect (qpack-blocked-stream-p next) :to-be t)
        (multiple-value-bind (released-p instruction)
            (qpack-cancel-blocked-stream adapter 7)
          (expect released-p :to-be t)
          (expect instruction :to-equalp (octets #x47)))
        (expect (hash-table-count
                 (http-message-kit::http3-qpack-decoder-context-blocked-streams adapter))
                :to-be 0)
        (expect (hash-table-count
                 (http-message-kit::http3-qpack-decoder-context-latest-streams adapter))
                :to-be 0))))


  (it "clears a ready token when its stream is cancelled"
    (let* ((encoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (capacity-wire (qpack-encode-set-capacity encoder 128))
           (insert-wire (qpack-encode-insert-with-literal-name
                         encoder "x-field" "value" :huffman nil))
           (instructions (concatenate '(vector (unsigned-byte 8))
                                      capacity-wire insert-wire))
           (token (qpack-decode-http3-field-section
                   (octets 2 0 #x80) adapter 8 :role :trailers)))
      (expect (qpack-blocked-stream-p token) :to-be t)
      (qpack-decode-encoder-stream decoder instructions)
      (let ((ready (qpack-take-ready-streams adapter)))
        (expect ready :to-equal (list token))
        (setf ready nil))
      (multiple-value-bind (released-p instruction)
          (qpack-cancel-blocked-stream adapter 8)
        (expect released-p :to-be t)
        (expect instruction :to-equalp (octets #x48)))
      (expect (hash-table-count
               (http-message-kit::http3-qpack-decoder-context-blocked-streams adapter))
              :to-be 0)
      (expect (hash-table-count
               (http-message-kit::http3-qpack-decoder-context-latest-streams adapter))
              :to-be 0)
      (expect (http-message-kit::http3-qpack-blocked-stream-resume-function token) :to-be nil)
      (signals http-protocol-error
        (qpack-resume-blocked-stream token))))

  (it "closes the decoder adapter and releases blocked-token references"
    (let* ((decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (token (qpack-decode-http3-field-section
                   (octets 2 0 #x80) adapter 9 :role :trailers)))
      (expect (qpack-blocked-stream-p token) :to-be t)
      (qpack-close-http3-decoder-context adapter)
      (expect (hash-table-count
               (http-message-kit::http3-qpack-decoder-context-blocked-streams adapter))
              :to-be 0)
      (expect (hash-table-count
               (http-message-kit::http3-qpack-decoder-context-latest-streams adapter))
              :to-be 0)
      (expect (http-message-kit::http3-qpack-blocked-stream-resume-function token) :to-be nil)
      (expect (http-message-kit::http3-qpack-blocked-stream-serialize token) :to-be nil)
      (expect (http-message-kit::http3-qpack-blocked-stream-context token) :to-be nil)
      (expect (http-message-kit::http3-qpack-decoder-context-closed-p adapter) :to-be t)
      (signals http-protocol-error
        (qpack-take-ready-streams adapter))))

  (it "treats SETTINGS_QPACK_BLOCKED_STREAMS overflow as decompression failure"
    (let* ((decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (first (qpack-decode-http3-field-section
                   (octets 2 0 #x80) adapter 3 :role :trailers))
           (failure nil))
      (expect (qpack-blocked-stream-p first) :to-be t)
      (handler-case
          (qpack-decode-http3-field-section
           (octets 2 0 #x80) adapter 7 :role :trailers)
        (http-connection-error (condition)
          (setf failure condition)))
      (expect (typep failure 'http-connection-error) :to-be t)
      (when (typep failure 'http-connection-error)
        (let ((cause (http-connection-error-cause failure)))
          (expect (typep cause 'http-protocol-error) :to-be t)
          (when (typep cause 'http-protocol-error)
            (expect (first (http-protocol-error-detail cause))
                    :to-be :qpack-decompression-failed))))
      (expect (hash-table-count
               (http-message-kit::http3-qpack-decoder-context-blocked-streams adapter))
              :to-be 1)
      (qpack-close-http3-decoder-context adapter)
      (expect (hash-table-count
               (http-message-kit::http3-qpack-decoder-context-blocked-streams adapter))
              :to-be 0)))


  (it "decodes the RFC 9204 Appendix B.4 section after Duplicate"
    (let* ((decoder (make-qpack-context :max-table-capacity 220
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (b2-encoder-stream
             (octets #x3f #xbd #x01
                     #xc0 #x0f
                     119 119 119 46 101 120 97 109 112 108 101 46 99 111 109
                     #xc1 #x0c
                     47 115 97 109 112 108 101 47 112 97 116 104))
           (b3-encoder-stream
             (octets #x4a
                     99 117 115 116 111 109 45 107 101 121
                     #x0c
                     99 117 115 116 111 109 45 118 97 108 117 101))
           (b4-field-section (octets #x05 #x00 #x80 #xc1 #x81)))
      (qpack-decode-encoder-stream decoder b2-encoder-stream)
      (qpack-decode-encoder-stream decoder b3-encoder-stream)
      (signals http-qpack-blocked
        (qpack-decode-header-block b4-field-section :context decoder))
      (qpack-decode-encoder-stream decoder (octets #x02))
      (multiple-value-bind (headers used required base)
          (qpack-decode-header-block b4-field-section :context decoder)
        (expect headers :to-equalp
                (list (make-http-pseudo-header ":authority" "www.example.com")
                      (make-http-pseudo-header ":path" "/")
                      (make-http-header "custom-key" "custom-value")))
        (expect used :to-be (length b4-field-section))
        (expect required :to-be 4)
        (expect base :to-be 4))))
  (it "acknowledges immediate dynamic sections after validation"
    (let* ((encoder (make-qpack-context :max-table-capacity 128
                                       :table-capacity 0
                                       :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (capacity-wire (qpack-encode-set-capacity encoder 128))
           (insert-wire (qpack-encode-insert-with-literal-name
                         encoder "x-field" "value" :huffman nil))
           (instructions
             (concatenate '(vector (unsigned-byte 8))
                          capacity-wire insert-wire))
           (wire (octets 2 0 #xd9 #x80))
           (acknowledged-streams nil))
      (qpack-decode-encoder-stream decoder instructions)
      (expect (qpack-decode-http3-field-section
               wire adapter 3 :role :response
               :on-section-acknowledgment
               (lambda (stream-id)
                 (push stream-id acknowledged-streams)))
              :to-equalp
              (list (cons ":status" "200") (cons "x-field" "value")))
      (expect (length acknowledged-streams) :to-be 1)
      (expect (first acknowledged-streams) :to-be 3)))
  (it "acknowledges a blocked dynamic section once after resume"
    (let* ((encoder (make-qpack-context :max-table-capacity 128
                                       :table-capacity 0
                                       :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (capacity-wire (qpack-encode-set-capacity encoder 128))
           (insert-wire (qpack-encode-insert-with-literal-name
                         encoder "x-field" "value" :huffman nil))
           (instructions
             (concatenate '(vector (unsigned-byte 8))
                          capacity-wire insert-wire))
           (acknowledged-streams nil)
           (blocked
             (qpack-decode-http3-field-section
              (octets 2 0 #x80) adapter 7 :role :trailers
              :on-section-acknowledgment
              (lambda (stream-id)
                (push stream-id acknowledged-streams)))))
      (expect (qpack-blocked-stream-p blocked) :to-be t)
      (expect acknowledged-streams :to-equal nil)
      (qpack-decode-encoder-stream decoder instructions)
      (expect (qpack-take-ready-streams adapter) :to-equal (list blocked))
      (expect (qpack-resume-blocked-stream blocked)
              :to-equalp (list (cons "x-field" "value")))
      (expect (length acknowledged-streams) :to-be 1)
      (expect (first acknowledged-streams) :to-be 7)
      (signals http-protocol-error
        (qpack-resume-blocked-stream blocked))
      (expect (length acknowledged-streams) :to-be 1)))
  (it "does not acknowledge a static-only field section"
    (let* ((decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (acknowledged-streams nil))
      (expect (qpack-decode-http3-field-section
               (octets 0 0 #xd9) adapter 11 :role :response
               :on-section-acknowledgment
               (lambda (stream-id)
                 (push stream-id acknowledged-streams)))
              :to-equalp (list (cons ":status" "200")))
      (expect acknowledged-streams :to-equal nil)))
  (it "does not acknowledge dynamic sections that fail role validation"
    (let* ((encoder (make-qpack-context :max-table-capacity 128
                                       :table-capacity 0
                                       :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (capacity-wire (qpack-encode-set-capacity encoder 128))
           (insert-wire (qpack-encode-insert-with-literal-name
                         encoder "x-field" "value" :huffman nil))
           (instructions
             (concatenate '(vector (unsigned-byte 8))
                          capacity-wire insert-wire))
           (acknowledged-streams nil))
      (qpack-decode-encoder-stream decoder instructions)
      (signals http-invalid-header
        (qpack-decode-http3-field-section
         (octets 2 0 #x80) adapter 13 :role :request
         :on-section-acknowledgment
         (lambda (stream-id)
           (push stream-id acknowledged-streams))))
      (expect acknowledged-streams :to-equal nil)))
  (it "does not acknowledge dynamic sections with uppercase names"
    (let* ((encoder (make-qpack-context :max-table-capacity 128
                                       :table-capacity 0
                                       :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (capacity-wire (qpack-encode-set-capacity encoder 128))
           (insert-wire (qpack-encode-insert-with-literal-name
                         encoder "x-field" "value" :huffman nil))
           (instructions
             (concatenate '(vector (unsigned-byte 8))
                          capacity-wire insert-wire))
           (uppercase-name-wire
             (octets 2 0 #x80 #x27
                     88 45 102 105 101 108 100
                     #x03 98 97 100))
           (acknowledged-streams nil))
      (qpack-decode-encoder-stream decoder instructions)
      (signals http-connection-error
        (qpack-decode-http3-field-section
         uppercase-name-wire adapter 17 :role :trailers
         :on-section-acknowledgment
         (lambda (stream-id)
           (push stream-id acknowledged-streams))))
      (expect acknowledged-streams :to-equal nil)))
  (it "calls the acknowledgment hook after the decode hook succeeds"
    (let* ((encoder (make-qpack-context :max-table-capacity 128
                                       :table-capacity 0
                                       :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :max-blocked-streams 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (capacity-wire (qpack-encode-set-capacity encoder 128))
           (insert-wire (qpack-encode-insert-with-literal-name
                         encoder "x-field" "value" :huffman nil))
           (instructions
             (concatenate '(vector (unsigned-byte 8))
                          capacity-wire insert-wire))
           (hook-completed-p nil)
           (acknowledged-streams nil))
      (qpack-decode-encoder-stream decoder instructions)
      (expect (http-message-kit::decode-http3-qpack-field-section
               (octets 2 0 #x80) adapter 19
               :on-decoded
               (lambda (fields)
                 (setf hook-completed-p t)
                 fields)
               :on-section-acknowledgment
               (lambda (stream-id)
                 (expect hook-completed-p :to-be t)
                 (push stream-id acknowledged-streams)))
              :to-equalp (list (cons "x-field" "value")))
      (expect (length acknowledged-streams) :to-be 1)
      (expect (first acknowledged-streams) :to-be 19))))
(describe "QPACK encoder transactions"
  (it "prepares capacity and insert instructions without changing context"
    (let ((context (make-qpack-context :max-table-capacity 256
                                      :table-capacity 0
                                      :huffman nil)))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-set-capacity context 256)
        (expect wire :to-equalp (octets #x3f #xe1 #x01))
        (expect (qpack-context-table-capacity context) :to-be 0)
        (qpack-commit-encoding transaction)
        (expect (qpack-context-table-capacity context) :to-be 256))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-insert-with-literal-name context "x-test" "one")
        (expect wire :to-equalp
                (octets #x46 120 45 116 101 115 116 #x03 111 110 101))
        (expect (qpack-context-insert-count context) :to-be 0)
        (expect (qpack-context-dynamic-entry-count context) :to-be 0)
        (qpack-commit-encoding transaction)
        (expect (qpack-context-insert-count context) :to-be 1))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-insert-with-name-reference
           context 0 "two" :static-p nil)
        (expect wire :to-equalp (octets #x80 #x03 116 119 111))
        (expect (qpack-context-insert-count context) :to-be 1)
        (qpack-commit-encoding transaction)
        (expect (qpack-context-insert-count context) :to-be 2))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-duplicate context 0)
        (expect wire :to-equalp (octets #x00))
        (expect (qpack-context-insert-count context) :to-be 2)
        (qpack-commit-encoding transaction)
        (expect (qpack-context-insert-count context) :to-be 3))))

  (it "prepares header sections without registration and commits references once"
    (let ((context (make-qpack-context :max-table-capacity 256
                                       :table-capacity 256
                                       :max-blocked-streams 1
                                       :huffman nil)))
      (qpack-encode-insert-with-literal-name context "x-test" "one"
                                             :huffman nil)
      (multiple-value-bind (wire transaction)
          (qpack-prepare-header-block
           (list (make-http-header "x-test" "one"))
           :context context :use-dynamic-p t :huffman nil :stream-id 5)
        (expect wire :to-equalp (octets #x02 #x00 #x80))
        (expect (qpack-context-insert-count context) :to-be 1)
        (expect (qpack-context-dynamic-entry-count context) :to-be 1)
        (qpack-commit-encoding transaction)
        (signals http-protocol-error
          (qpack-context-set-capacity context 0))
        (progn
          (expect (qpack-decode-decoder-stream context (octets #x85))
                  :to-equalp '((:section-ack 5)))
          (qpack-context-set-capacity context 0))
        (expect (qpack-context-table-capacity context) :to-be 0)
        (expect (qpack-context-dynamic-entry-count context) :to-be 0))))

  (it "aborts capacity, insertion, and section changes without mutating context"
    (let ((context (make-qpack-context :max-table-capacity 256
                                       :table-capacity 256
                                       :huffman nil)))
      (qpack-encode-insert-with-literal-name context "x-test" "one"
                                             :huffman nil)
      (multiple-value-bind (wire transaction)
          (qpack-prepare-set-capacity context 128)
        (declare (ignore wire))
        (expect (qpack-abort-encoding transaction) :to-be t)
        (expect (qpack-context-table-capacity context) :to-be 256))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-insert-with-literal-name context "x-new" "two")
        (declare (ignore wire))
        (qpack-abort-encoding transaction)
        (expect (qpack-context-insert-count context) :to-be 1))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-header-block
           (list (make-http-header "x-test" "one"))
           :context context :use-dynamic-p t :huffman nil :stream-id 7)
        (declare (ignore wire))
        (qpack-abort-encoding transaction)
        (let ((reason nil))
          (handler-case
              (qpack-commit-encoding transaction)
            (qpack-encoding-transaction-error (condition)
              (setf reason
                    (qpack-encoding-transaction-error-reason condition))))
          (expect reason :to-be :aborted))
        (expect (qpack-context-table-capacity context) :to-be 256)
        (progn
          (expect (qpack-context-dynamic-entry-count context) :to-be 1)
          (qpack-context-set-capacity context 0)
          (expect (qpack-context-table-capacity context) :to-be 0)
          (expect (qpack-context-dynamic-entry-count context) :to-be 0)))))

  (it "rejects stale and repeated transaction commits"
    (let ((context (make-qpack-context :max-table-capacity 256
                                       :table-capacity 0
                                       :huffman nil)))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-set-capacity context 64)
        (declare (ignore wire))
        (multiple-value-bind (events used)
            (qpack-decode-encoder-stream context (octets #x3f #x61))
          (expect events :to-equalp '((:set-capacity 128)))
          (expect used :to-be 2))
        (let ((reason nil))
          (handler-case
              (qpack-commit-encoding transaction)
            (qpack-encoding-transaction-error (condition)
              (setf reason
                    (qpack-encoding-transaction-error-reason condition))))
          (expect reason :to-be :stale))
        (expect (qpack-context-table-capacity context) :to-be 128))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-set-capacity context 64)
        (declare (ignore wire))
        (qpack-context-set-capacity context 32)
        (let ((reason nil))
          (handler-case
              (qpack-commit-encoding transaction)
            (qpack-encoding-transaction-error (condition)
              (setf reason
                    (qpack-encoding-transaction-error-reason condition))))
          (expect reason :to-be :stale))
        (expect (qpack-context-table-capacity context) :to-be 32))
      (progn
  (qpack-context-set-capacity context 64)
  (qpack-encode-insert-with-literal-name context "x" "v" :huffman nil))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-set-capacity context 32)
        (declare (ignore wire))
        (qpack-decode-decoder-stream context (octets #x01))
        (let ((reason nil))
          (handler-case
              (qpack-commit-encoding transaction)
            (qpack-encoding-transaction-error (condition)
              (setf reason
                    (qpack-encoding-transaction-error-reason condition))))
          (expect reason :to-be :stale))
        (expect (qpack-context-known-received-count context) :to-be 1))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-set-capacity context 64)
        (declare (ignore wire))
        (qpack-commit-encoding transaction)
        (let ((reason nil))
          (handler-case
              (qpack-commit-encoding transaction)
            (qpack-encoding-transaction-error (condition)
              (setf reason
                    (qpack-encoding-transaction-error-reason condition))))
          (expect reason :to-be :committed))
        (expect (qpack-context-table-capacity context) :to-be 64))))

  (it "validates capacity and insertion changes during preparation"
    (let ((context (make-qpack-context :max-table-capacity 64
                                       :table-capacity 0
                                       :huffman nil)))
      (signals http-protocol-error
        (qpack-prepare-set-capacity context 65))
      (expect (qpack-context-table-capacity context) :to-be 0)
      (qpack-context-set-capacity context 64)
      (signals http-protocol-error
        (qpack-prepare-insert-with-literal-name
         context "x" (make-string 40 :initial-element #\a)))
      (expect (qpack-context-insert-count context) :to-be 0)
      (expect (qpack-context-table-size context) :to-be 0)))
  (it "requires a stream ID for dynamic header references"
    (let* ((context (make-qpack-context :max-table-capacity 256
                                        :table-capacity 256
                                        :max-blocked-streams 1
                                        :huffman nil))
           (headers (list (make-http-header "x-test" "one"))))
      (qpack-encode-insert-with-literal-name context "x-test" "one"
                                             :huffman nil)
      (multiple-value-bind (wire transaction)
          (qpack-prepare-header-block headers :context context :huffman nil)
        (multiple-value-bind (decoded used required base)
            (qpack-decode-header-block wire :context context)
          (expect decoded :to-equalp headers)
          (expect used :to-be (length wire))
          (expect required :to-be 0)
          (expect base :to-be 0))
        (qpack-commit-encoding transaction))
      (signals http-protocol-error
        (qpack-prepare-header-block
         headers :context context :use-dynamic-p t :huffman nil))
      (expect (qpack-context-insert-count context) :to-be 1)
      (expect (qpack-context-dynamic-entry-count context) :to-be 1)
      (multiple-value-bind (wire transaction)
          (qpack-prepare-header-block
           headers :context context :use-dynamic-p t
           :stream-id 9 :huffman nil)
        (expect wire :to-equalp (octets #x02 #x00 #x80))
        (qpack-abort-encoding transaction))))
  (it "updates the context and dynamic-table maximum together"
    (let ((context (make-qpack-context :max-table-capacity 0
                                       :table-capacity 0
                                       :huffman nil)))
      (expect (qpack-context-max-table-capacity context) :to-be 0)
      (expect (qpack-context-table-capacity context) :to-be 0)
      (qpack-context-set-max-table-capacity context 256)
      (expect (qpack-context-max-table-capacity context) :to-be 256)
      (expect (qpack-context-table-capacity context) :to-be 0)
      (qpack-context-set-capacity context 128)
      (expect (setf (qpack-context-max-table-capacity context) 512)
              :to-be 512)
      (expect (qpack-context-max-table-capacity context) :to-be 512)
      (expect (qpack-context-table-capacity context) :to-be 128)
      (qpack-context-set-capacity context 512)
      (expect (qpack-context-table-capacity context) :to-be 512)
      (signals http-protocol-error
        (qpack-context-set-capacity context 513))
      (expect (qpack-context-table-capacity context) :to-be 512)))
  (it "rejects maximums below current capacity and out-of-range values"
    (let ((context (make-qpack-context :max-table-capacity 128
                                       :table-capacity 64
                                       :huffman nil)))
      (signals http-protocol-error
        (qpack-context-set-max-table-capacity context 63))
      (expect (qpack-context-max-table-capacity context) :to-be 128)
      (expect (qpack-context-table-capacity context) :to-be 64)
      (signals http-protocol-error
        (setf (qpack-context-max-table-capacity context) 63))
      (expect (qpack-context-max-table-capacity context) :to-be 128)
      (expect (qpack-context-table-capacity context) :to-be 64)
      (signals http-protocol-error
        (qpack-context-set-max-table-capacity context #x4000000000000000))
      (expect (qpack-context-max-table-capacity context) :to-be 128)
      (expect (qpack-context-table-capacity context) :to-be 64)
      (qpack-context-set-capacity context 0)
      (qpack-context-set-max-table-capacity context 63)
      (expect (qpack-context-max-table-capacity context) :to-be 63)
      (expect (qpack-context-table-capacity context) :to-be 0)))
  (it "invalidates prepared encodings when maximum capacity changes"
    (let ((context (make-qpack-context :max-table-capacity 128
                                       :table-capacity 0
                                       :huffman nil)))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-set-capacity context 64)
        (declare (ignore wire))
        (qpack-context-set-max-table-capacity context 256)
        (let ((reason nil))
          (handler-case
              (qpack-commit-encoding transaction)
            (qpack-encoding-transaction-error (condition)
              (setf reason
                    (qpack-encoding-transaction-error-reason condition))))
          (expect reason :to-be :stale))
        (expect (qpack-context-max-table-capacity context) :to-be 256)
        (expect (qpack-context-table-capacity context) :to-be 0))))
  (it "accepts zero and the largest 62-bit maximum capacity"
    (let ((context (make-qpack-context :max-table-capacity 1
                                       :table-capacity 0
                                       :huffman nil)))
      (qpack-context-set-max-table-capacity context 0)
      (expect (qpack-context-max-table-capacity context) :to-be 0)
      (expect (qpack-context-table-capacity context) :to-be 0)
      (signals http-protocol-error
        (qpack-context-set-capacity context 1))
      (expect (qpack-context-table-capacity context) :to-be 0)
      (qpack-context-set-max-table-capacity context #x3fffffffffffffff)
      (expect (qpack-context-max-table-capacity context)
              :to-be #x3fffffffffffffff)
      (qpack-context-set-capacity context #x3fffffffffffffff)
      (expect (qpack-context-table-capacity context)
              :to-be #x3fffffffffffffff))))
(describe "QPACK wire boundary contracts"
  (it "returns octet vectors, decodes empty sections, and rejects malformed references"
    (let ((wire
            (qpack-encode-header-block
             (list (make-http-header "x-test" "value"))
             :huffman nil)))
      (expect (array-element-type wire) :to-equalp '(unsigned-byte 8)))
    (multiple-value-bind (headers used required base)
        (qpack-decode-header-block (octets 0 0))
      (expect headers :to-equalp nil)
      (expect used :to-be 2)
      (expect required :to-be 0)
      (expect base :to-be 0))
    (signals http-protocol-error
      (qpack-decode-header-block (octets #x01 #x00)))
    (signals http-protocol-error
      (qpack-decode-header-block (octets 0 0 #x10))))

  (it "accepts digit-prefixed names and rejects empty pseudo-names"
    (let* ((header (make-http-header "9X-0123456789" "value"))
           (wire (qpack-encode-header-block (list header) :huffman nil)))
      (expect (qpack-decode-header-block wire)
              :to-equalp
              (list (make-http-header "9x-0123456789" "value"))))
    (signals http-protocol-error
      (qpack-decode-header-block
       (octets #x00 #x00 #x21 #x3a #x05 118 97 108 117 101))))

  (it "wraps required insert counts and rejects invalid prefixes"
    (let ((context (make-qpack-context :max-table-capacity 64
                                       :table-capacity 64
                                       :max-blocked-streams 1
                                       :huffman nil))
          (headers (list (make-http-header "x" "v"))))
      (loop for count from 1 to 8
            do (qpack-encode-insert-with-literal-name
                context "x" "v" :huffman nil)
               (let ((wire (qpack-encode-header-block
                            headers :context context :use-dynamic-p t
                            :stream-id count :huffman nil)))
                 (expect (aref wire 0) :to-be (1+ (mod count 4)))
                 (multiple-value-bind (decoded used required base)
                     (qpack-decode-header-block
                      wire :context context :stream-id count)
                   (expect decoded :to-equalp headers)
                   (expect used :to-be (length wire))
                   (expect required :to-be count)
                   (expect base :to-be count)))
               (qpack-decode-decoder-stream
                context (qpack-encode-section-ack count)))
      (signals http-protocol-error
        (qpack-decode-header-block (octets #x05 #x00) :context context))))

  (it "encodes and decodes Delta Base signs at 7-bit integer boundaries"
    (let ((context (make-qpack-context :max-table-capacity 8192
                                       :table-capacity 8192
                                       :max-blocked-streams 2
                                       :huffman nil)))
      (loop for index from 0 below 128
            do (qpack-encode-insert-with-literal-name
                context "x" (format nil "v~D" index) :huffman nil))
      (expect (qpack-encode-header-block
               (list (make-http-header "x" "v0"))
               :context context :use-dynamic-p t :stream-id 1 :huffman nil)
              :to-equalp (octets #x02 #x7f #x00 #xbf #x40))
      (qpack-encode-insert-with-literal-name
       context "x" "v128" :huffman nil)
      (expect (qpack-encode-header-block
               (list (make-http-header "x" "v0"))
               :context context :use-dynamic-p t :stream-id 2 :huffman nil)
              :to-equalp (octets #x02 #x7f #x01 #xbf #x41)))
    (let ((context (make-qpack-context :max-table-capacity 64
                                       :table-capacity 64
                                       :huffman nil)))
      (qpack-encode-insert-with-literal-name context "x" "v" :huffman nil)
      (dolist (spec
                (list (list (octets #x02 #x7f #x00 #xbf #x40) 1 128)
                      (list (octets #x02 #x7f #x01 #xbf #x41) 1 129)))
        (multiple-value-bind (headers used required base)
            (qpack-decode-header-block (first spec) :context context)
          (expect headers :to-equalp (list (make-http-header "x" "v")))
          (expect used :to-be (length (first spec)))
          (expect required :to-be (second spec))
          (expect base :to-be (third spec)))))
    (let ((context (make-qpack-context :max-table-capacity 16384
                                       :table-capacity 16384
                                       :huffman nil)))
      (loop for index from 0 below 257
            do (qpack-encode-insert-with-literal-name
                context "x" (format nil "v~D" index) :huffman nil))
      (dolist (spec
                (list (list (octets #xff #x02 #xff #x00 #xbf #x40) 256 128)
                      (list (octets #xff #x03 #xff #x01 #xbf #x40) 257 128)))
        (multiple-value-bind (headers used required base)
            (qpack-decode-header-block (first spec) :context context)
          (expect headers :to-equalp (list (make-http-header "x" "v0")))
          (expect used :to-be (length (first spec)))
          (expect required :to-be (second spec))
          (expect base :to-be (third spec))))))

  (it "reports blocked insert diagnostics and resumes after insertion"
    (let* ((encoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 128
                                        :max-blocked-streams 1
                                        :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 128
                                        :max-blocked-streams 1
                                        :huffman nil))
           (headers (list (make-http-header "x-blocked" "yes")))
           (insert-wire (qpack-encode-insert-with-literal-name
                         encoder "x-blocked" "yes" :huffman nil))
           (wire (qpack-encode-header-block
                  headers :context encoder :use-dynamic-p t
                  :stream-id 7 :huffman nil))
           (blocked-p nil))
      (handler-case
          (qpack-decode-header-block wire :context decoder :stream-id 7)
        (http-qpack-blocked (condition)
          (setf blocked-p t)
          (expect (http-qpack-blocked-required-insert-count condition)
                  :to-be 1)
          (expect (http-qpack-blocked-available-insert-count condition)
                  :to-be 0)
          (expect (http-qpack-blocked-stream-id condition) :to-be 7)
          (expect (http-error-message condition)
                  :to-equal
                  "QPACK field section is blocked on dynamic-table insertions.")
          (expect (princ-to-string condition)
                  :to-equal
                  "QPACK field section is blocked on dynamic-table insertions.")))
      (expect blocked-p :to-be t)
      (expect (qpack-context-insert-count decoder) :to-be 0)
      (multiple-value-bind (events used)
          (qpack-decode-encoder-stream decoder insert-wire)
        (expect events :to-equalp
                (list (list :insert "x-blocked" "yes" 0 :huffman nil)))
        (expect used :to-be (length insert-wire)))
      (expect (qpack-context-insert-count decoder) :to-be 1)
      (expect (qpack-decode-header-block
               wire :context decoder :stream-id 7)
              :to-equalp headers)))

  (it "keeps protected entries and insertion state unchanged on eviction failure"
    (let* ((context (make-qpack-context :max-table-capacity 68
                                        :table-capacity 68
                                        :max-blocked-streams 1
                                        :huffman nil))
           (headers (list (make-http-header "x" "a"))))
      (qpack-encode-insert-with-literal-name context "x" "a" :huffman nil)
      (qpack-encode-insert-with-literal-name context "y" "b" :huffman nil)
      (multiple-value-bind (wire transaction)
          (qpack-prepare-header-block
           headers :context context :use-dynamic-p t :stream-id 5
           :huffman nil)
        (qpack-commit-encoding transaction)
        (expect (qpack-decode-header-block
                 wire :context context :stream-id 5)
                :to-equalp headers))
      (signals http-protocol-error
        (qpack-prepare-insert-with-literal-name context "z" "c"))
      (expect (qpack-context-insert-count context) :to-be 2)
      (expect (qpack-context-dynamic-entry-count context) :to-be 2)
      (expect (qpack-context-table-size context) :to-be 68)
      (multiple-value-bind (wire transaction)
          (qpack-prepare-header-block
           headers :context context :use-dynamic-p t :stream-id 5
           :huffman nil)
        (qpack-commit-encoding transaction)
        (expect (qpack-decode-header-block
                 wire :context context :stream-id 5)
                :to-equalp headers))
      (multiple-value-bind (events used)
          (qpack-decode-decoder-stream context (octets #x85))
        (expect events :to-equalp (list (list :section-ack 5)))
        (expect used :to-be 1))
      (signals http-protocol-error
        (qpack-context-set-capacity context 34))
      (expect (qpack-context-table-capacity context) :to-be 68)
      (expect (qpack-context-table-size context) :to-be 68)
      (expect (qpack-context-dynamic-entry-count context) :to-be 2)
      (multiple-value-bind (events used)
          (qpack-decode-decoder-stream context (octets #x85))
        (expect events :to-equalp (list (list :section-ack 5)))
        (expect used :to-be 1))
      (multiple-value-bind (wire transaction)
          (qpack-prepare-insert-with-literal-name context "z" "c")
        (declare (ignore wire))
        (qpack-commit-encoding transaction))
      (expect (qpack-context-insert-count context) :to-be 3)
      (expect (qpack-context-dynamic-entry-count context) :to-be 2)
      (expect (qpack-context-table-size context) :to-be 68)))

  (it "retains shared references until every section is acknowledged or cancelled"
    (let* ((context (make-qpack-context :max-table-capacity 68
                                        :table-capacity 68
                                        :max-blocked-streams 2
                                        :huffman nil))
           (headers (list (make-http-header "x" "a"))))
      (qpack-encode-insert-with-literal-name context "x" "a" :huffman nil)
      (qpack-encode-insert-with-literal-name context "y" "b" :huffman nil)
      (dolist (stream-id (list 1 3))
        (multiple-value-bind (wire transaction)
            (qpack-prepare-header-block
             headers :context context :use-dynamic-p t
             :stream-id stream-id :huffman nil)
          (qpack-commit-encoding transaction)
          (expect (qpack-decode-header-block
                   wire :context context :stream-id stream-id)
                  :to-equalp headers)))
      (expect (qpack-decode-decoder-stream
               context (qpack-encode-section-ack 1))
              :to-equalp (list (list :section-ack 1)))
      (let ((failure nil)
            (known-received-count
              (qpack-context-known-received-count context)))
        (handler-case
            (qpack-decode-decoder-stream
             context (qpack-encode-section-ack 1))
          (http-protocol-error (condition)
            (setf failure condition)))
        (expect (and failure (http-protocol-error-detail failure))
                :to-equalp
                (list :qpack-decoder-stream-error
                      :section-acknowledgment 1))
        (expect (qpack-context-known-received-count context)
                :to-be known-received-count))
      (expect (qpack-context-table-capacity context) :to-be 68)
      (expect (qpack-context-table-size context) :to-be 68)
      (expect (qpack-context-dynamic-entry-count context) :to-be 2)
      (signals http-protocol-error
        (qpack-context-set-capacity context 34))
      (expect (qpack-context-table-capacity context) :to-be 68)
      (expect (qpack-context-table-size context) :to-be 68)
      (expect (qpack-context-dynamic-entry-count context) :to-be 2)
      (expect (qpack-decode-decoder-stream
               context (qpack-encode-stream-cancellation 3))
              :to-equalp (list (list :stream-cancellation 3)))
      (qpack-context-set-capacity context 34)
      (expect (qpack-context-table-capacity context) :to-be 34)
      (expect (qpack-context-table-size context) :to-be 34)
      (expect (qpack-context-dynamic-entry-count context) :to-be 1)))

  (it "never indexes sensitive values when matching dynamic names exist"
    (dolist (huffman (list nil t))
      (let ((context (make-qpack-context :max-table-capacity 256
                                         :table-capacity 256
                                         :max-blocked-streams 1
                                         :huffman huffman)))
        (qpack-encode-insert-with-literal-name
         context "proxy-authorization" "replacement" :huffman nil)
        (qpack-encode-insert-with-literal-name
         context "x-unrelated" "value" :huffman nil)
        (let* ((headers (list (make-http-header
                               "proxy-authorization" "replacement")))
               (before-size (qpack-context-table-size context))
               (wire (qpack-encode-header-block
                      headers :context context :use-dynamic-p t :stream-id 17
                      :huffman huffman))
               (field-line (aref wire 2))
               (n-bit-mask (if (logbitp 6 field-line) #x20 #x10)))
          (expect (logbitp 7 field-line) :to-be nil)
          (expect (logand field-line n-bit-mask) :to-be n-bit-mask)
          (expect (qpack-decode-header-block wire :context context)
                  :to-equalp headers)
          (expect (qpack-context-insert-count context) :to-be 2)
          (expect (qpack-context-dynamic-entry-count context) :to-be 2)
          (expect (qpack-context-table-size context) :to-be before-size)))))

  (it "rejects unavailable and evicted dynamic references"
    (signals http-protocol-error
      (qpack-decode-header-block (octets #x02 #x00 #x80)))
    (let* ((encoder (make-qpack-context :max-table-capacity 64
                                        :table-capacity 64
                                        :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 64
                                        :table-capacity 0
                                        :huffman nil))
           (capacity-wire (qpack-encode-set-capacity encoder 64))
           (old-insert (qpack-encode-insert-with-literal-name
                        encoder "x-old" "v" :huffman nil))
           (new-insert (qpack-encode-insert-with-literal-name
                        encoder "x-new" "v" :huffman nil))
           (instructions
             (concatenate '(vector (unsigned-byte 8))
                          capacity-wire old-insert new-insert)))
      (qpack-decode-encoder-stream decoder instructions)
      (expect (qpack-context-insert-count decoder) :to-be 2)
      (expect (qpack-context-dynamic-entry-count decoder) :to-be 1)
      (expect (qpack-context-table-size decoder) :to-be 38)
      (signals http-protocol-error
        (qpack-decode-header-block
         (octets #x02 #x00 #x81) :context decoder))
      (expect (qpack-context-insert-count decoder) :to-be 2)
      (expect (qpack-context-dynamic-entry-count decoder) :to-be 1)
      (expect (qpack-context-table-size decoder) :to-be 38)))

  (it "limits blocked references using the known received insert count"
    (dolist (increment (list nil t))
      (let* ((context (make-qpack-context :max-table-capacity 128
                                          :table-capacity 128
                                          :max-blocked-streams 1
                                          :huffman nil))
             (first-fields (list (make-http-header "x-a" "one")))
             (second-fields (list (make-http-header "x-b" "two"))))
        (qpack-encode-insert-with-literal-name context "x-a" "one" :huffman nil)
        (qpack-encode-insert-with-literal-name context "x-b" "two" :huffman nil)
        (let ((wire (qpack-encode-header-block
                     first-fields :context context :use-dynamic-p t
                     :stream-id 4 :huffman nil)))
          (expect (subseq wire 0 2) :to-equalp (octets 2 1))
          (expect (qpack-decode-header-block wire :context context)
                  :to-equalp first-fields))
        (when increment
          (expect (qpack-decode-decoder-stream
                   context (qpack-encode-insert-count-increment 1))
                  :to-equalp (list (list :insert-count-increment 1))))
        (let ((wire (qpack-encode-header-block
                     second-fields :context context :use-dynamic-p t
                     :stream-id 8 :huffman nil)))
          (expect (subseq wire 0 2)
                  :to-equalp (if increment (octets 3 0) (octets 0 0)))
          (expect (qpack-decode-header-block wire :context context)
                  :to-equalp second-fields))
        (expect (qpack-context-known-received-count context)
                :to-be (if increment 1 0))
        (expect (qpack-context-insert-count context) :to-be 2)
        (expect (qpack-context-dynamic-entry-count context) :to-be 2)
        (expect (qpack-decode-decoder-stream
                 context (qpack-encode-section-ack 4))
                :to-equalp (list (list :section-ack 4)))
        (expect (qpack-context-known-received-count context) :to-be 1)
        (if increment
            (progn
              (signals http-protocol-error
                (qpack-context-set-capacity context 0))
              (expect (qpack-context-table-capacity context) :to-be 128)
              (expect (qpack-context-table-size context) :to-be 76)
              (expect (qpack-context-dynamic-entry-count context) :to-be 2)
              (expect (qpack-decode-decoder-stream
                       context (qpack-encode-section-ack 8))
                      :to-equalp (list (list :section-ack 8))))
            (qpack-context-set-capacity context 0))
        (when increment
          (qpack-context-set-capacity context 0))
        (expect (qpack-context-table-capacity context) :to-be 0)
        (expect (qpack-context-table-size context) :to-be 0)
        (expect (qpack-context-dynamic-entry-count context) :to-be 0)))))

(describe "QPACK downstream test support"
  (it "preserves the first dynamic-entry identity when a protected capacity shrink fails"
    (let* ((context (make-qpack-context :max-table-capacity 68
                                        :table-capacity 68
                                        :max-blocked-streams 1
                                        :huffman nil))
           (headers (list (make-http-header "y" "b")))
           (wire nil)
           (same-first-entry-p nil))
      (qpack-encode-insert-with-literal-name context "x" "a" :huffman nil)
      (qpack-encode-insert-with-literal-name context "y" "b" :huffman nil)
      (multiple-value-bind (prepared-wire transaction)
          (qpack-prepare-header-block
           headers :context context :use-dynamic-p t :stream-id 5 :huffman nil)
        (qpack-commit-encoding transaction)
        (setf wire prepared-wire))
      (setf same-first-entry-p
            (http-message-kit/test-support:make-qpack-first-dynamic-entry-identity-probe
             context))
      (signals http-protocol-error
        (qpack-context-set-capacity context 0))
      (expect (qpack-context-table-capacity context) :to-be 68)
      (expect (qpack-context-table-size context) :to-be 68)
      (expect (qpack-context-dynamic-entry-count context) :to-be 2)
      (expect (funcall same-first-entry-p) :to-be t)
      (expect (qpack-decode-header-block wire :context context :stream-id 5)
              :to-equalp headers)
      (multiple-value-bind (events used)
          (qpack-decode-decoder-stream context (octets #x85))
        (expect events :to-equalp (list (list :section-ack 5)))
        (expect used :to-be 1))
      (qpack-encode-insert-with-literal-name context "z" "c" :huffman nil)
      (expect (funcall same-first-entry-p) :to-be nil)))
  (it "observes the decoded callback once on resume and restores the decoder after unwind"
    (let* ((encoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :huffman nil))
           (decoder (make-qpack-context :max-table-capacity 128
                                        :table-capacity 0
                                        :huffman nil))
           (adapter (qpack-make-http3-decoder-context
                     decoder :max-blocked-streams 1))
           (expected-fields (list (cons "x-field" "value")))
           (capacity-wire (qpack-encode-set-capacity encoder 128))
           (insert-wire
             (qpack-encode-insert-with-literal-name
              encoder "x-field" "value" :huffman nil))
           (instructions (concatenate '(vector (unsigned-byte 8))
                                      capacity-wire insert-wire))
           (wire (octets 2 0 #x80))
           (serialize-calls 0)
           (inside-serialize-p nil)
           (wired-callbacks nil)
           (observed-fields nil)
           (observed-inside-serialize-p nil)
           (acknowledged-streams nil)
           (decoded
             (http-message-kit/test-support:call-with-http3-qpack-decoded-callback-observer
              (lambda ()
                (let ((blocked
                        (qpack-decode-http3-field-section
                         wire adapter 19 :role :trailers
                         :serialize
                         (lambda (thunk)
                           (incf serialize-calls)
                           (let ((previous inside-serialize-p))
                             (setf inside-serialize-p t)
                             (unwind-protect (funcall thunk)
                               (setf inside-serialize-p previous))))
                         :on-section-acknowledgment
                         (lambda (stream-id)
                           (push stream-id acknowledged-streams)))))
                  (expect (qpack-blocked-stream-p blocked) :to-be t)
                  (qpack-decode-encoder-stream decoder instructions)
                  (expect (qpack-take-ready-streams adapter)
                          :to-equal (list blocked))
                  (qpack-resume-blocked-stream blocked)))
              :on-wired (lambda (callback)
                          (push callback wired-callbacks))
              :on-decoded (lambda (fields)
                            (push fields observed-fields)
                            (setf observed-inside-serialize-p
                                  inside-serialize-p)))))
      (expect decoded :to-equalp expected-fields)
      (expect serialize-calls :to-be 3)
      (expect (length wired-callbacks) :to-be 1)
      (expect (functionp (first wired-callbacks)) :to-be t)
      (expect observed-fields :to-equalp (list expected-fields))
      (expect observed-inside-serialize-p :to-be nil)
      (expect acknowledged-streams :to-equal (list 19))
      (let ((wiring-after-unwind 0)
            (static-headers (list (make-http-header "x-restored" "yes"))))
        (signals simple-error
          (http-message-kit/test-support:call-with-http3-qpack-decoded-callback-observer
           (lambda () (error "intentional observer unwind"))
           :on-wired (lambda (callback)
                       (when (functionp callback)
                         (incf wiring-after-unwind)))))
        (expect (qpack-decode-http3-field-section
                 (qpack-encode-header-block static-headers :huffman nil)
                 adapter 21 :role :trailers)
                :to-equalp (list (cons "x-restored" "yes")))
        (expect wiring-after-unwind :to-be 0)))))
