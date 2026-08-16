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
    (signals http-invalid-header (make-http-header "bad name" "x")))

  (it "trims optional whitespace around field content"
    (let ((content (format nil "~C value ~C" #\Tab #\Tab)))
      (expect (http-header-content (make-http-header "X-Value" content))
              :to-equalp "value")))

  (it "rejects non-ASCII names and control characters"
    (signals http-invalid-header (make-http-header "X-☃" "value"))
    (signals http-invalid-header
      (make-http-header "X-Value" (format nil "bad~Cvalue" #\Newline))))

  (it "copies its public strings and supports explicit copies"
    (let* ((header (make-http-header "X-Value" "value"))
           (name (http-header-name header))
           (content (http-header-content header))
           (copy (http-header-copy header)))
      (setf (char name 0) #\X)
      (setf (char content 0) #\X)
      (expect (http-header-name header) :to-equalp "x-value")
      (expect (http-header-content header) :to-equalp "value")
      (expect (http-header-name copy) :to-equalp "x-value"))))

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
    (let ((headers (list (make-http-header "Accept" "text/plain")
                         (make-http-header "accept" "text/html"))))
      (expect (http-header-values headers "accept")
              :to-equalp (list "text/plain" "text/html"))))

  (it "normalises name/value pairs and preserves their order"
    (let ((headers (list (cons "X-Trace" "one")
                         (list "x-trace" "two"))))
      (expect (http-header-values headers "X-TRACE")
              :to-equalp (list "one" "two"))))

  (it "rejects malformed header lists"
    (signals http-invalid-header
      (http-header-values (list (cons "X-Trace" (list "one" "two")))
                          "x-trace"))
    (signals http-invalid-header
      (http-header-values (cons (make-http-header "X-Trace" "one")
                                (make-http-header "X-Other" "two"))
                          "x-trace")))

  (it "redacts sensitive values when printed"
    (let ((printed (princ-to-string (make-http-header "Authorization" "secret"))))
      (expect (and (search "<redacted>" printed) t) :to-equalp t)
      (expect (search "secret" printed) :to-equalp nil))))
