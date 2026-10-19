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
           (context (make-qpack-context :huffman nil))
           (instructions
             (list
              (list (qpack-encode-section-ack stream-id)
                    (list (list :section-ack stream-id)))
              (list (qpack-encode-stream-cancellation stream-id)
                    (list (list :stream-cancellation stream-id))))))
      (dolist (spec instructions)
        (multiple-value-bind (events used)
            (qpack-decode-decoder-stream context (first spec))
          (expect events :to-equalp (second spec))
          (expect used :to-be (length (first spec)))))))

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
        (expect base :to-be 0)))))

(describe "QPACK blocked-stream snapshots"
  (it "resumes from original field-section bytes after caller mutation"
    (let* ((wire (octets 2 0 #x80))
           (decoder-table (http-message-kit::make-qpack-dynamic-table :max-capacity 128))
           (context
             (http-message-kit::make-http3-qpack-decoder-context
              decoder-table :blocked-stream-limit 1))
           (blocked
             (http-message-kit::decode-http3-qpack-field-section
              wire context 3)))
      (expect (http-message-kit::http3-qpack-blocked-stream-p blocked)
              :to-be t)
      (setf (aref wire 2) #x81)
      (http-message-kit::qpack-dynamic-table-insert decoder-table "x-original" "v")
      (expect (http-message-kit::%http3-qpack-detach-ready-streams context)
              :to-equalp (list blocked))
      (expect (http-message-kit::resume-http3-qpack-blocked-stream blocked)
              :to-equalp (list (cons "x-original" "v"))))))
