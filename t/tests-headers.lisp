(in-package #:http-message-kit/test)

(describe "make-http-header"
  ;; Field names are case-insensitive on the wire, so they are normalised once
  ;; at construction and every later comparison is a plain string compare.
  (it "downcases the field name and keeps the content verbatim"
    (let ((header (make-http-header "Content-Type" "text/plain")))
      (expect (http-header-name header) :to-equalp "content-type")
      (expect (http-header-content header) :to-equalp "text/plain")))

  (it "rejects an empty field name"
    (signals http-invalid-header (make-http-header "" "x")))

  (it "rejects a field name containing a space"
    (signals http-invalid-header (make-http-header "bad name" "x"))))

(describe "header lookup"
  (it "finds a value regardless of the case asked for"
    (let ((headers (list (make-http-header "Accept" "text/plain"))))
      (expect (http-header-value headers "ACCEPT") :to-equalp "text/plain")))

  (it "returns the supplied default when the field is absent"
    (let ((headers (list (make-http-header "Accept" "text/plain"))))
      (expect (http-header-value headers "missing" :none) :to-equalp :none)))

  (it "reports presence and absence"
    (let ((headers (list (make-http-header "X-Trace" "abc"))))
      (expect (and (http-header-present-p headers "x-trace") t) :to-equalp t)
      (expect (and (http-header-present-p headers "missing") t) :to-equalp nil)))

  (it "collects every value recorded for a name"
    (let ((headers (list (make-http-header "Accept" "text/plain"))))
      (expect (http-header-values headers "accept")
              :to-equalp (list "text/plain")))))
