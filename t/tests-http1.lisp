(in-package #:http-message-kit/test)

(defun wire-string (string)
  (let ((result (make-array (length string)
                            :element-type '(unsigned-byte 8))))
    (loop for index below (length string)
          do (setf (aref result index) (char-code (char string index))))
    result))

(defun wire-http1 (&rest lines)
  (wire-string
   (with-output-to-string (stream)
     (dolist (line lines)
       (write-string line stream)
       (write-char #\return stream)
       (write-char #\linefeed stream)))))

(describe "incremental HTTP/1 parser"
  (it "retains split requests and returns the next message remainder"
    (let* ((wire (concatenate 'vector
                              (wire-http1 "GET / HTTP/1.1"
                                          "Host: example.com"
                                          "Content-Length: 5"
                                          "")
                              (wire-string "helloNEXT")))
           (parser (make-http1-parser :message-type :request)))
      (multiple-value-bind (message remainder complete-p)
          (http1-parser-feed parser (subseq wire 0 7))
        (expect message :to-be nil)
        (expect remainder :to-equalp (octets))
        (expect complete-p :to-be nil)
        (expect (http1-parser-buffered-bytes parser) :to-be 7))
      (multiple-value-bind (message remainder complete-p)
          (http1-parser-feed parser (subseq wire 7))
        (expect complete-p :to-be-truthy)
        (expect (http-request-method message) :to-equalp "GET")
        (expect (http-request-target message) :to-equalp "/")
        (expect (http-request-body message) :to-equalp (wire-string "hello"))
        (expect remainder :to-equalp (wire-string "NEXT"))
        (expect (http1-parser-remainder parser) :to-equalp (wire-string "NEXT")))))

  (it "finishes a close-delimited response only at end of input"
    (let ((parser (make-http1-parser :message-type :response)))
      (multiple-value-bind (message remainder complete-p)
          (http1-parser-feed
           parser
           (concatenate 'vector
                        (wire-http1 "HTTP/1.1 200 OK" "")
                        (wire-string "abc")))
        (expect message :to-be nil)
        (expect remainder :to-equalp (octets))
        (expect complete-p :to-be nil))
      (multiple-value-bind (message remainder complete-p)
          (http1-parser-finish parser)
        (expect complete-p :to-be-truthy)
        (expect (http-response-status message) :to-be 200)
        (expect (http-response-body message) :to-equalp (wire-string "abc"))
        (expect remainder :to-equalp (octets)))))

  (it "reports an incomplete request at the final input boundary"
    (let ((parser (make-http1-parser :message-type :request)))
      (http1-parser-feed parser (wire-http1 "GET / HTTP/1.1")
                         :end-of-input nil)
      (signals http-incomplete-message
        (http1-parser-finish parser))))

  (it "enforces the header limit before a split terminator arrives"
    (let ((parser (make-http1-parser :message-type :request
                                     :max-header-bytes 7)))
      (signals http-size-limit-exceeded
        (http1-parser-feed parser (wire-string "GET / HT")))))

  (it "can be reset after a complete message"
    (let ((parser (make-http1-parser :message-type :request)))
      (http1-parser-feed parser
                         (wire-http1 "GET / HTTP/1.1"
                                     "Host: example.test"
                                     ""))
      (expect (http1-parser-complete-p parser) :to-be-truthy)
      (http1-parser-reset parser)
      (expect (http1-parser-complete-p parser) :to-be nil)
      (expect (http1-parser-buffered-bytes parser) :to-be 0))))

(describe "HTTP/1 framing contracts"
  (it "adds Host from the request URI and parses it back"
    (let ((wire (http1-serialize-request
                 (make-http-request :method "GET"
                                    :uri "http://example.test/"))))
      (multiple-value-bind (request remainder)
          (http1-parse-request wire)
        (expect (http-request-authority request) :to-equalp "example.test")
        (expect (http-header-value (http-request-headers request) "host")
                :to-equalp "example.test")
        (expect remainder :to-equalp (octets)))))

  (it "rejects duplicate Host fields on serialization"
    (signals http-protocol-error
      (http1-serialize-request
       (make-http-request
        :method "GET"
        :uri "http://example.test/"
        :headers (list (make-http-header "Host" "a.example")
                       (make-http-header "Host" "b.example"))))))

  (it "rejects a missing Host field in an HTTP/1.1 request"
    (signals http-protocol-error
      (http1-parse-request (wire-http1 "GET / HTTP/1.1" ""))))

  (it "rejects framing fields on a 204 response"
    (signals http-protocol-error
      (http1-serialize-response
       (make-http-response
        :status 204
        :headers (list (make-http-header "Content-Length" "0")))))
    (signals http-protocol-error
      (http1-serialize-response
       (make-http-response
        :status 204
        :headers (list (make-http-header "Transfer-Encoding" "chunked"))))))

  (it "rejects framing fields and payload metadata on CONNECT tunnels"
    (signals http-protocol-error
      (http1-serialize-response
       (make-http-response
        :status 200
        :headers (list (make-http-header "Content-Length" "0")))
       :request-method "CONNECT"))
    (signals http-protocol-error
      (http1-serialize-response
       (make-http-response
        :status 200
        :headers (list (make-http-header "Transfer-Encoding" "chunked")))
       :request-method "CONNECT"))
    (signals http-protocol-error
      (http1-serialize-response
       (make-http-response :status 200 :body (octets 1))
       :request-method "CONNECT"))
    (signals http-protocol-error
      (http1-serialize-response
       (make-http-response
        :status 200
        :trailers (list (make-http-header "X-Trailer" "value")))
       :request-method "CONNECT"))))

  (it "allows the hypothetical Content-Length on a 304 response"
    (let ((wire (http1-serialize-response
                 (make-http-response
                  :status 304
                  :headers (list (make-http-header "Content-Length" "123")))
                 :request-method "GET")))
      (multiple-value-bind (response remainder)
          (http1-parse-response wire :request-method "GET")
        (expect (http-header-value (http-response-headers response)
                                   "content-length")
                :to-equalp "123")
        (expect remainder :to-equalp (octets)))))
