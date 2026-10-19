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

  (it "accepts visible targets and defers form validation to HTTP/1"
    (dolist (target (list "/path#fragment"
                          "https://example.test/path#fragment"
                          "example.test"
                          "example.test:not-a-port"))
      (let ((request (make-http-request :method "GET"
                                        :uri "https://example.test/"
                                        :request-target target)))
        (expect (http-request-target request) :to-equalp target)
        (signals http-protocol-error
          (http1-serialize-request request))))
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
  (it "reports the original request target as the protocol error detail"
    (dolist (code (list 0 9 10 13 31 32 127 128 #x100))
      (let* ((target (format nil "/~C" (code-char code)))
             (condition
               (handler-case
                   (make-http-request :method "GET"
                                      :uri "https://example.test/"
                                      :request-target target)
                 (http-protocol-error (condition) condition))))
        (expect (typep condition (quote http-protocol-error)) :to-be t)
        (when (typep condition (quote http-protocol-error))
          (expect (http-error-operation condition) :to-equalp :request)
          (expect (http-protocol-error-detail condition) :to-equalp target)))))

  (it "accepts the supported HTTP protocol versions"
    (dolist (version '("HTTP/1.0" "HTTP/1.1" "HTTP/2" "HTTP/3"))
      (expect (http-request-protocol-version
               (make-http-request :method "GET"
                                  :uri "https://example.test/"
                                  :protocol-version version))
              :to-equalp version)))
  (it "shares protocol-version strings across requests and streams"
      (let* ((version (copy-seq "HTTP/1.1"))
             (request (make-http-request :method "GET"
                                         :uri "https://example.test/"
                                         :protocol-version version))
             (request-stream
              (make-http-request-stream
               :method "GET"
               :uri "https://example.test/"
               :protocol-version (http-request-protocol-version request)
               :body-function (lambda () nil)))
             (response-stream
              (make-http-response-stream
               :status 200
               :protocol-version (http-request-protocol-version request))))
        (expect (eq version (http-request-protocol-version request)) :to-equalp t)
        (expect (eq version (http-request-stream-protocol-version request-stream))
                :to-equalp t)
        (expect (eq version (http-response-stream-protocol-version response-stream))
                :to-equalp t)
        (setf (char (http-request-protocol-version request) 7) #\9)
        (expect version :to-equalp "HTTP/1.9")
        (expect (http-request-stream-protocol-version request-stream)
                :to-equalp "HTTP/1.9")
        (expect (http-response-stream-protocol-version response-stream)
                :to-equalp "HTTP/1.9")))
  (it "retains aliased header names and returns independent header copies"
    (let* ((name (copy-seq "X-Request"))
           (request (make-http-request
                     :method "GET"
                     :uri "https://example.test/"
                     :headers (list (make-http-header name "value"))))
           (returned (first (http-request-headers request))))
      (setf (char (http-header-name returned) 0) #\Y)
      (expect name :to-equalp "X-Request")
      (setf (char name 0) #\x)
      (expect (http-header-name (first (http-request-headers request)))
              :to-equalp "x-Request")
      (setf (char name 0) #\Space)
      (signals http-invalid-header (http-request-headers request)))))

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
    (expect (http-status-reason 418) :to-equalp "I'm a teapot")
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

  (it "preserves the original response stream body function"
    (let* ((producer (lambda () (octets 79 75)))
           (response (make-http-response-stream
                      :status 200
                      :body-length 2
                      :body-function producer)))
      (expect (eq producer (http-response-stream-body-function response))
              :to-be-truthy)
      (expect (funcall (http-response-stream-body-function response))
              :to-equalp (octets 79 75))
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
                                 :body-length -1)))
  (it "preserves response protocol-version identity"
      (let* ((version (copy-seq "HTTP/1.1"))
             (response (make-http-response :status 200
                                           :protocol-version version))
             (stream (make-http-response-stream :status 200
                                                :protocol-version version)))
        (expect (eq version (http-response-protocol-version response)) :to-equalp t)
        (expect (eq version (http-response-stream-protocol-version stream))
                :to-equalp t)
        (setf (char (http-response-protocol-version response) 7) #\9)
        (expect version :to-equalp "HTTP/1.9")
        (expect (http-response-stream-protocol-version stream) :to-equalp "HTTP/1.9")))
  (it "retains aliased header names and returns independent header copies"
    (let* ((name (copy-seq "X-Response"))
           (response (make-http-response
                      :status 200
                      :headers (list (make-http-header name "value"))))
           (copy (http-header-copy (make-http-header name "value")))
           (returned (first (http-response-headers response))))
      (setf (char (http-header-name returned) 0) #\Y)
      (expect name :to-equalp "X-Response")
      (setf (char name 0) #\x)
      (expect (http-header-name (first (http-response-headers response)))
              :to-equalp "x-Response")
      (expect (http-header-name copy) :to-equalp "X-Response")
      (setf (char name 0) #\Space)
      (signals http-invalid-header (http-response-headers response))))
  (it "retains aliased header names through response streams"
    (let* ((name (copy-seq "X-Stream"))
           (response (make-http-response-stream
                      :status 200
                      :headers (list (make-http-header name "value")))))
      (setf (char name 0) #\x)
      (expect (http-header-name (first (http-response-stream-headers response)))
              :to-equalp "x-Stream")
      (setf (char name 0) #\Space)
      (signals http-invalid-header (http-response-stream-headers response)))))
(describe "HTTP message compatibility edge cases"
  (it "rejects content on TRACE requests while preserving method case"
    (signals http-protocol-error
      (make-http-request :method "TRACE"
                         :uri "https://example.test/"
                         :body (octets 1)))
    (expect (http-request-body
             (make-http-request :method "trace"
                                :uri "https://example.test/"
                                :body (octets 1)))
            :to-equalp (octets 1)))
  (it "uses the matching reason phrases for 306 and 418"
    (expect (http-status-reason 306) :to-equalp "Switch Proxy")
    (expect (http-status-reason 418) :to-equalp "I'm a teapot")
    (expect (http-response-reason (make-http-response :status 306))
            :to-equalp "Switch Proxy")
    (expect (http-response-reason (make-http-response :status 418))
            :to-equalp "I'm a teapot")))
