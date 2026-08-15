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
      (expect (http-request-protocol-version request) :to-equalp "HTTP/1.1"))))

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
    (signals http-invalid-status (make-http-response :status 99))))
