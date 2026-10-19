(in-package #:http-message-kit/test)

(defun p4a-wire-string (string)
  (map '(vector (unsigned-byte 8)) #'char-code string))

(describe "Deferred validation boundaries"
  (it "rejects malformed request targets at HTTP/1 and HTTP/2/3 boundaries"
    (let* ((target "/bad%escape")
           (request (make-http-request :method "GET"
                                       :uri "https://example.test/"
                                       :request-target target)))
      (expect (http-request-target request) :to-equalp target)
      (signals http-protocol-error
        (http1-serialize-request request)))
    (signals http-protocol-error
      (http1-parse-request
       (p4a-wire-string
        (concatenate (quote string)
                     "GET /bad%escape HTTP/1.1"
                     (format nil "~C~C" #\return #\linefeed)
                     "Host: example.test"
                     (format nil "~C~C~C~C"
                             #\return #\linefeed #\return #\linefeed)))))
    (dolist (validator '(http2-validate-header-section
                         http3-validate-header-section))
      (signals http-protocol-error
        (funcall validator
                 (list (make-http-pseudo-header ":method" "GET")
                       (make-http-pseudo-header ":scheme" "https")
                       (make-http-pseudo-header ":authority" "example.test")
                       (make-http-pseudo-header ":path" "/bad%escape"))
                 :request-p t))))

  (it "accepts body and trailers for bodyless statuses and rejects them in HTTP/1"
    (dolist (status '(100 204 304))
      (let ((response (make-http-response :status status :body (octets 1))))
        (expect (http-response-body response) :to-equalp (octets 1))
        (signals http-invalid-framing (http1-serialize-response response)))
      (let ((response (make-http-response
                       :status status
                       :trailers (list (make-http-header "x-end" "done")))))
        (expect (length (http-response-trailers response)) :to-be 1)
        (signals http-invalid-framing (http1-serialize-response response)))
      (let* ((producer (lambda () (octets 1)))
             (response (make-http-response-stream
                        :status status
                        :trailers (list (make-http-header "x-end" "done"))
                        :body-function producer
                        :body-length 1)))
        (expect (eq producer (http-response-stream-body-function response))
                :to-be-truthy)
        (expect (length (http-response-stream-trailers response)) :to-be 1))))

  (it "validates request and response trailer field-name syntax and defers prohibited fields to codecs"
    (let* ((name (copy-seq ":status"))
           (request-failure
             (handler-case
                 (make-http-request :method "POST"
                                    :uri "https://example.test/"
                                    :trailers (list (cons name "value")))
               (http-invalid-header (condition) condition)))
           (response-failure
             (handler-case
                 (make-http-response :status 200
                                     :trailers (list (cons name "value")))
               (http-invalid-header (condition) condition))))
      (dolist (failure (list request-failure response-failure))
        (expect (typep failure 'http-invalid-header) :to-be t)
        (when (typep failure 'http-invalid-header)
          (expect (http-error-operation failure) :to-equalp :header)
          (expect (http-invalid-header-reason failure) :to-equalp :name)
          (expect (eq name (http-invalid-header-name failure)) :to-be t)
          (expect (http-error-message failure)
                  :to-equalp "Header names must be non-empty ASCII tokens.")))
      (let* ((trailers (list (make-http-header "Content-Length" "1")))
             (headers (list (make-http-header "Transfer-Encoding" "chunked")))
             (request (make-http-request :method "POST"
                                         :uri "https://example.test/"
                                         :headers headers
                                         :body (octets 65)
                                         :trailers trailers)))
        (expect (length (http-request-trailers request)) :to-be 1)
        (signals http-invalid-header
          (http1-serialize-request request))
        (signals http-invalid-header
          (http1-decode-chunked
           (p4a-wire-string
            (concatenate (quote string)
                         "0"
                         (format nil "~C~C" #\return #\linefeed)
                         "Content-Length: 1"
                         (format nil "~C~C~C~C"
                                 #\return #\linefeed #\return #\linefeed)))))
        (let ((response (make-http-response :status 200
                                            :headers headers
                                            :trailers trailers)))
          (expect (length (http-response-trailers response)) :to-be 1)
          (signals http-invalid-header
            (http1-serialize-response response)))
        (dolist (validator '(http2-validate-header-section
                             http3-validate-header-section))
          (signals http-protocol-error
            (funcall validator trailers :trailers-p t)))))))
