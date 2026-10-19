(in-package #:http-message-kit/test)

(describe "make-http-header"
  (it "preserves field-name case"
    (let ((header (make-http-header "Content-Type" "text/plain")))
      (expect (http-header-name header) :to-equalp "Content-Type")
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

  (it "exposes raw header values and keeps request copies isolated"
    (let* ((header (make-http-header "X-Value" "value"))
           (copy (http-header-copy header))
           (request (make-http-request :method "GET"
                                       :uri "https://example.test/"
                                       :headers (list header)))
           (new-name (copy-seq "X-Updated"))
           (new-content (copy-seq "updated")))
      (expect (eq (setf (http-header-name header) new-name) new-name)
              :to-equalp t)
      (expect (eq (http-header-name header) new-name) :to-equalp t)
      (expect (eq (setf (http-header-content header) new-content) new-content)
              :to-equalp t)
      (expect (eq (http-header-content header) new-content) :to-equalp t)
      (setf (char new-name 0) #\x
            (char new-content 0) #\U)
      (expect (http-header-name header) :to-equalp "x-Updated")
      (expect (http-header-content header) :to-equalp "Updated")
      (expect (http-header-value (list header) "x-updated")
              :to-equalp "Updated")
      (expect (http-header-name copy) :to-equalp "X-Value")
      (expect (http-header-content copy) :to-equalp "value")
      (let ((exposed (first (http-request-headers request))))
        (expect (http-header-name exposed) :to-equalp "X-Value")
        (expect (http-header-content exposed) :to-equalp "value")
        (setf (char (http-header-name exposed) 0) #\x
              (char (http-header-content exposed) 0) #\X)
        (expect (http-header-name (first (http-request-headers request)))
                :to-equalp "X-Value")
        (expect (http-header-content (first (http-request-headers request)))
                :to-equalp "value"))
      (let ((invalid-name "bad name"))
        (expect (eq (setf (http-header-name header) invalid-name) invalid-name)
                :to-equalp t)
        (expect (eq (http-header-name header) invalid-name) :to-equalp t)
        (signals http-invalid-header
          (make-http-request :method "GET"
                             :uri "https://example.test/"
                             :headers (list header))))
      (setf (http-header-name header) "X-Updated")
      (expect (eql (setf (http-header-content header) 7) 7) :to-be-truthy)
      (expect (eql (http-header-content header) 7) :to-be-truthy)
      (signals http-invalid-header
        (make-http-request :method "GET"
                           :uri "https://example.test/"
                           :headers (list header)))
      (setf (http-header-content header) "updated")
      (expect (http-header-content header) :to-equalp "updated"))))

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

  (it "accepts name/value pairs case-insensitively and preserves order"
    (let ((headers (list (cons "X-Trace" "one")
                         (list "x-trace" "two"))))
      (expect (http-header-values headers "X-TRACE")
              :to-equalp (list "one" "two"))))

  (it "rejects malformed value pairs and improper header lists"
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
