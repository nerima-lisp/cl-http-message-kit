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
    (signals http-invalid-uri (parse-http-uri 42)))

  (it "normalises the scheme and host while preserving the URI spelling"
    (let ((uri (parse-http-uri "HTTPS://Example.TEST:443/a%20b?x=1")))
      (expect (http-uri-scheme uri) :to-equalp "https")
      (expect (http-uri-host uri) :to-equalp "example.test")
      (expect (http-uri-authority uri) :to-equalp "example.test:443")
      (expect (http-uri-string uri) :to-equalp
              "https://example.test:443/a%20b?x=1")))

  (it "preserves an explicitly empty query"
    (let ((uri (parse-http-uri "http://example.test/path?")))
      (expect (http-uri-query uri) :to-equalp "")
      (expect (http-uri-string uri) :to-equalp "http://example.test/path?")))

  (it "does not expose mutable URI storage"
    (let ((uri (parse-http-uri "http://example.test/path?q=1")))
      (let ((path (http-uri-path uri))
            (query (http-uri-query uri)))
        (setf (char path 1) #\X)
        (setf (char query 0) #\X))
      (expect (http-uri-path uri) :to-equalp "/path")
      (expect (http-uri-query uri) :to-equalp "q=1")))

  (it "accepts IPv4, IPv6, IPvFuture, and zone identifiers"
    (dolist (input '("http://127.0.0.1:8080/"
                     "http://[2001:db8::1]/"
                     "http://[v1.fe80]/"
                     "http://[fe80::1%25eth0]/"
                     "http://[fe80::1%25eth0%2Fwan]/"))
      (expect (http-uri-p (parse-http-uri input)) :to-equalp t)))

  (it "rejects malformed authority and URI components"
    (dolist (input '("http://01.2.3.4/"
                     "http://2001:db8::1/"
                     "http://[2001:db8:::1]/"
                     "http://[2001:db8::1"
                     "http://example.test/%ZZ"
                     "http://example.test/a b"
                     "http://example.test/a#fragment"))
      (signals http-invalid-uri (parse-http-uri input)))))
