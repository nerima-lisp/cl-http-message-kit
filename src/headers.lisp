(in-package #:http-message-kit)

(defparameter *http-forbidden-trailer-fields*
  '("connection" "content-length" "host" "keep-alive" "proxy-authenticate"
    "proxy-authentication-info" "proxy-authorization" "te" "trailer"
    "transfer-encoding" "upgrade")
  "Field names that cannot be carried in an HTTP trailer section.")

(defstruct (http-header (:constructor %make-http-header)
                        (:conc-name %header-))
  name
  content)

(defun http-header-name (header)
  (check-type header http-header)
  (%header-name header))

(defun (setf http-header-name) (name header)
  (check-type header http-header)
  (setf (%header-name header) name)
  name)

(defun http-header-content (header)
  (check-type header http-header)
  (%header-content header))
(defun (setf http-header-content) (content header)
  (check-type header http-header)
  (setf (%header-content header) content)
  content)

(defun http-pseudo-header-p (header)
  (and (http-header-p header)
       (let ((name (%header-name header)))
         (and (plusp (length name))
              (char= (char name 0) #\:)))))

(defun %pseudo-header-name-p (name)
  (and (stringp name)
       (> (length name) 1)
       (char= (char name 0) #\:)
       (%token-p (subseq name 1))))

(defun %sensitive-header-p (name)
  (member (%ascii-lowercase name) *redacted-header-names* :test #'string=))

(defun make-http-header (name content &key pseudo-p)
  (unless (and (stringp name)
               (if pseudo-p
                   (%pseudo-header-name-p name)
                   (%header-name-p name)))
    (error 'http-invalid-header
           :message (if pseudo-p
                        "Pseudo-header names must be a colon followed by an ASCII token."
                        "Header names must be non-empty ASCII tokens.")
           :operation :header
           :name name
           :reason :name))
  (unless (stringp content)
    (error 'http-invalid-header
           :message "Header values must be strings."
           :operation :header
           :name name
           :reason :value-type))
  (unless (%header-value-p content)
    (error 'http-invalid-header
           :message "Header values cannot contain controls or CRLF."
           :operation :header
           :name name
           :reason :control))
  (%make-http-header :name (if pseudo-p
                               (concatenate 'string ":"
                                            (%ascii-lowercase (subseq name 1)))
                               name)
                     :content (%trim-ows content)))

(defun make-http-pseudo-header (name content)
  (make-http-header name content :pseudo-p t))

(defun http-trailer-field-p (header)
  (and (http-header-p header)
       (not (http-pseudo-header-p header))
       (not (member (%ascii-lowercase (%header-name header))
                    *http-forbidden-trailer-fields*
                    :test #'string=))))

(declaim (ftype function %normalize-headers))

(defun http-validate-trailers (trailers)
  (let ((normalized (%normalize-headers trailers)))
    (dolist (header normalized normalized)
      (unless (http-trailer-field-p header)
        (error 'http-invalid-header
               :message "The field is not permitted in an HTTP trailer section."
               :operation :trailer
               :name (http-header-name header)
               :reason :forbidden-trailer-field)))))

(defun %copy-http-header (header)
  (check-type header http-header)
  (make-http-header (copy-seq (%header-name header))
                    (%header-content header)
                    :pseudo-p (http-pseudo-header-p header)))

(defun http-header-copy (header)
  (%copy-http-header header))

(defmethod print-object ((header http-header) stream)
  (print-unreadable-object (header stream :type t)
    (format stream "~A: ~A"
            (%header-name header)
            (if (%sensitive-header-p (%header-name header))
                "<redacted>"
                (%bounded-diagnostic (%header-content header))))))

(defun %invalid-header-list ()
  (error 'http-invalid-header
         :message "Headers must be a proper list of HTTP-HEADER values or name/value pairs."
         :operation :header
         :reason :type))

(defun %normalize-headers (headers)
  (unless (%proper-list-p headers)
    (%invalid-header-list))
  (mapcar (lambda (header)
            (cond ((http-header-p header) (%copy-http-header header))
                  ((and (consp header) (stringp (car header)))
                   (let ((tail (cdr header)))
                     (cond ((stringp tail)
                            (make-http-header (car header) tail
                                              :pseudo-p (%pseudo-header-name-p
                                                         (car header))))
                           ((and (consp tail)
                                 (stringp (car tail))
                                 (null (cdr tail)))
                            (make-http-header (car header) (car tail)
                                              :pseudo-p (%pseudo-header-name-p
                                                         (car header))))
                           (t
                            (%invalid-header-list)))))
                  (t
                   (%invalid-header-list))))
          (or headers '())))

(defun %header-name-equal-p (left right)
  (string= (%ascii-lowercase left) (%ascii-lowercase right)))

(defun http-header-values (headers name)
  (unless (and (stringp name)
               (or (%header-name-p name)
                   (%pseudo-header-name-p name)))
    (error 'http-invalid-header
           :message "Header lookup requires an ASCII field name."
           :operation :header-lookup
           :name name
           :reason :name))
  (let ((normalized (%normalize-headers headers)))
    (loop for header in normalized
          when (%header-name-equal-p name (%header-name header))
            collect (copy-seq (%header-content header)))))

(defun http-header-value (headers name &optional default)
  (let ((values (http-header-values headers name)))
    (if values
        (first values)
        default)))

(defun http-header-present-p (headers name)
  (not (null (http-header-values headers name))))
