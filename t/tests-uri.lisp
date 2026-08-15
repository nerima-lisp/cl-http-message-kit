(in-package #:http-message-kit/test)

(describe "parse-http-uri"
  (it "decomposes an absolute URI into its components"
    (let ((uri (parse-http-uri "https://example.test:8443/a/b?q=1")))
      (expect (http-uri-scheme uri) :to-equalp "https")
      (expect (http-uri-host uri) :to-equalp "example.test")
      (expect (http-uri-port uri) :to-equalp 8443)
      (expect (http-uri-authority uri) :to-equalp "example.test:8443")
      (expect (http-uri-path uri) :to-equalp "/a/b")
      (expect (http-uri-query uri) :to-equalp "q=1")))

  (it "preserves the original text"
    (expect (http-uri-string (parse-http-uri "https://example.test:8443/a/b?q=1"))
            :to-equalp "https://example.test:8443/a/b?q=1"))

  ;; An absent port stays absent rather than being filled in from the scheme:
  ;; the default belongs to whoever opens the connection, not to the value.
  (it "leaves an absent port and query unset"
    (let ((uri (parse-http-uri "http://example.test/")))
      (expect (http-uri-port uri) :to-equalp nil)
      (expect (http-uri-query uri) :to-equalp nil)
      (expect (http-uri-path uri) :to-equalp "/")))

  (it "rejects input with no scheme"
    (signals http-invalid-uri (parse-http-uri "example.test/a")))

  (it "rejects input that is not a string"
    (signals http-invalid-uri (parse-http-uri 42))))
