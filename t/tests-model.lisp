(in-package #:http-message-kit/test)

(describe "make-http-request"
  (it "derives the authority, path and query from the URI"
    (let ((request (make-http-request
                    :method "GET"
                    :uri (parse-http-uri "https://example.test/health?x=1"))))
      (expect (http-request-method request) :to-equalp "GET")
      (expect (http-request-authority request) :to-equalp "example.test")
      (expect (http-request-path request) :to-equalp "/health")
      (expect (http-request-query request) :to-equalp "x=1")))

  ;; The request target is the origin-form the HTTP/1.1 request line carries:
  ;; path and query only, never the scheme or authority.
  (it "derives an origin-form request target"
    (let ((request (make-http-request
                    :method "GET"
                    :uri (parse-http-uri "https://example.test/health?x=1"))))
      (expect (http-request-target request) :to-equalp "/health?x=1")))

  (it "defaults the protocol version"
    (let ((request (make-http-request
                    :method "GET"
                    :uri (parse-http-uri "https://example.test/"))))
      (expect (http-request-protocol-version request) :to-equalp "HTTP/1.1")))

  (it "preserves method case, body bytes, headers, and trailers"
    (let* ((request (make-http-request
                     :method "get"
                     :uri "https://example.test/"
                     :headers (list (cons "X-Request" "yes"))
                     :trailers (list (list "X-Trailer" "done"))
                     :body (octets 1 2 3)))
           (body (http-request-body request)))
      (setf (aref body 0) 9)
      (expect (http-request-method request) :to-equalp "get")
      (expect (http-request-body request) :to-equalp (octets 1 2 3))
      (expect (http-header-value (http-request-headers request) "x-request")
              :to-equalp "yes")
      (expect (http-header-value (http-request-trailers request) "x-trailer")
              :to-equalp "done")))

  (it "classifies all request-target forms"
    (let ((uri "https://example.test/path"))
      (expect (http-request-target-form
               (make-http-request :method "GET" :uri uri :request-target "*"))
              :to-equalp :asterisk)
      (expect (http-request-target-form
               (make-http-request :method "GET" :uri uri :request-target "/path?q=1"))
              :to-equalp :origin)
      (expect (http-request-target-form
               (make-http-request :method "GET" :uri uri
                                  :request-target "HTTPS://example.test/path"))
              :to-equalp :absolute)
      (expect (http-request-target-form
              (make-http-request :method "CONNECT" :uri uri
                                  :request-target "example.test:443"))
              :to-equalp :authority)))

  (it "validates request-target syntax and exposes stream target forms"
    (dolist (target '("/path#fragment"
                      "https://example.test/path#fragment"
                      "example.test"
                      "example.test:not-a-port"))
      (signals http-protocol-error
        (make-http-request :method "GET"
                           :uri "https://example.test/"
                           :request-target target)))
    (let ((stream (make-http-request-stream
                   :method "CONNECT"
                   :uri "https://example.test/"
                   :request-target "example.test:443"
                   :body-function (lambda () nil))))
      (expect (http-request-stream-target-form stream) :to-equalp :authority)))

  (it "preserves an empty query in the derived request target"
    (let ((request (make-http-request
                    :method "GET"
                    :uri "https://example.test/path?")))
      (expect (http-request-target request) :to-equalp "/path?")
      (expect (http-request-target-form request) :to-equalp :origin)))

  (it "validates methods, targets, protocols, and protocol versions"
    (signals http-protocol-error
      (make-http-request :method "bad method" :uri "https://example.test/"))
    (signals http-protocol-error
      (make-http-request :method "GET" :uri "https://example.test/"
                         :request-target "bad target"))
    (signals http-protocol-error
      (make-http-request :method "GET" :uri "https://example.test/"
                         :protocol-version "HTTP/9"))
    (let* ((protocol (copy-seq "WebSocket"))
           (request (make-http-request :method "GET"
                                       :uri "https://example.test/"
                                       :protocol protocol))
           (returned (http-request-protocol request)))
      (setf (char protocol 0) #\x
            (char returned 0) #\x)
      (expect (http-request-protocol request) :to-equalp "websocket"))
    (expect (http-request-protocol
             (make-http-request :method "GET" :uri "https://example.test/"))
            :to-equalp nil)
    (signals http-protocol-error
      (make-http-request :method "GET" :uri "https://example.test/"
                         :protocol 42))
    (signals http-protocol-error
      (make-http-request :method "GET" :uri "https://example.test/"
                         :protocol "bad protocol")))

  (it "accepts the supported HTTP protocol versions"
    (dolist (version '("HTTP/1.0" "HTTP/1.1" "HTTP/2" "HTTP/3"))
      (expect (http-request-protocol-version
               (make-http-request :method "GET"
                                  :uri "https://example.test/"
                                  :protocol-version version))
              :to-equalp version))))

(describe "make-http-response"
  (it "keeps the status and body and supplies the registered reason"
    (let ((response (make-http-response :status 200 :body (octets 79 75))))
      (expect (http-response-status response) :to-equalp 200)
      (expect (http-response-reason response) :to-equalp "OK")
      (expect (http-response-body response) :to-equalp (octets 79 75))))

  (it "exposes its headers to the lookup functions"
    (let ((response (make-http-response
                     :status 200
                     :headers (list (make-http-header "Content-Type" "text/plain")))))
      (expect (http-header-value (http-response-headers response) "content-type")
              :to-equalp "text/plain")))

  (it "rejects a status outside the valid range"
    (signals http-invalid-status (make-http-response :status 99)))

  (it "covers registered reasons and status classes"
    (expect (http-status-reason 104) :to-equalp "Upload Resumption Supported")
    (expect (http-status-reason 506) :to-equalp "Variant Also Negotiates")
    (expect (http-status-reason 508) :to-equalp "Loop Detected")
    (expect (http-status-reason 418) :to-equalp "")
    (expect (http-information-status-p 199) :to-equalp t)
    (expect (http-success-status-p 204) :to-equalp t)
    (expect (http-redirection-status-p 302) :to-equalp t)
    (expect (http-client-error-status-p 404) :to-equalp t)
    (expect (http-server-error-status-p 503) :to-equalp t)
    (expect (http-server-error-status-p 499) :to-equalp nil))

  (it "accepts custom reasons and rejects reason controls"
    (let ((response (make-http-response :status 299 :reason "Custom")))
      (expect (http-response-reason response) :to-equalp "Custom"))
    (signals http-invalid-status
      (make-http-response :status 200
                          :reason (format nil "bad~Creason" #\Newline)))
    (signals http-invalid-status
      (make-http-response :status 200 :reason 42)))

  (it "copies response body and exposes trailers"
    (let* ((response (make-http-response
                      :status 200
                      :headers (list (cons "Content-Type" "text/plain"))
                      :trailers (list (list "X-Trailer" "done"))
                      :body (octets 79 75)))
           (body (http-response-body response)))
      (setf (aref body 0) 0)
      (expect (http-response-body response) :to-equalp (octets 79 75))
      (expect (http-header-value (http-response-headers response) "content-type")
              :to-equalp "text/plain")
      (expect (http-header-value (http-response-trailers response) "x-trailer")
              :to-equalp "done")))

  (it "streams a request body and enforces its declared length"
    (let* ((chunks (list (octets 1 2) (list 3) nil))
           (request (make-http-request-stream
                     :method "POST"
                     :uri "https://example.test/upload"
                     :body-length 3
                     :body-function (lambda () (pop chunks))))
           (next (http-request-stream-body-function request)))
      (expect (funcall next) :to-equalp (octets 1 2))
      (expect (funcall next) :to-equalp (octets 3))
      (expect (funcall next) :to-equalp nil)
      (expect (funcall next) :to-equalp nil)
      (expect (http-request-stream-body-length request) :to-equalp 3)))

  (it "rejects malformed and incomplete request body streams"
    (let* ((chunks (list (cons 1 2) nil))
           (request (make-http-request-stream
                     :method "POST"
                     :uri "https://example.test/upload"
                     :body-function (lambda () (pop chunks)))))
      (signals http-protocol-error
        (funcall (http-request-stream-body-function request))))
    (let* ((chunks (list (octets 1) nil))
           (request (make-http-request-stream
                     :method "POST"
                     :uri "https://example.test/upload"
                     :body-length 2
                     :body-function (lambda () (pop chunks)))))
      (funcall (http-request-stream-body-function request))
      (signals http-protocol-error
        (funcall (http-request-stream-body-function request)))))

  (it "streams a response body and validates overflow"
    (let* ((chunks (list (octets 79 75) (octets 33) nil))
           (response (make-http-response-stream
                      :status 200
                      :body-length 2
                      :body-function (lambda () (pop chunks))))
           (next (http-response-stream-body-function response)))
      (expect (funcall next) :to-equalp (octets 79 75))
      (signals http-protocol-error (funcall next))
      (expect (http-response-stream-status response) :to-equalp 200)))

  (it "checks size limits and deadlines"
    (expect (http-check-size-limit 4 4 :headers) :to-equalp 4)
    (signals http-size-limit-exceeded
      (http-check-size-limit 5 4 :body))
    (signals http-protocol-error
      (http-check-size-limit -1 4))
    (expect (http-deadline 2 :clock-function (lambda () 10)) :to-equalp 12)
    (expect (http-deadline 2 :deadline 11 :clock-function (lambda () 10))
            :to-equalp 11)
    (signals http-timeout
      (with-http-deadline (deadline 0 :clock-function (lambda () 1) :kind :read)
        deadline)))

  (it "accepts a missing response body function and rejects invalid stream arguments"
    (signals http-protocol-error
      (make-http-request-stream :method "GET" :uri "https://example.test/"
                                :body-function nil))
    (let ((response (make-http-response-stream :status 200 :body-function nil)))
      (expect (http-response-stream-body-function response) :to-equalp nil))
    (signals http-protocol-error
      (make-http-response-stream :status 200 :body-function :invalid))
    (signals http-protocol-error
      (make-http-response-stream :status 200 :body-function (lambda () nil)
                                 :body-length -1))))
