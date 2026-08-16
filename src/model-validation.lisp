(in-package #:http-message-kit)

(defun %validate-protocol-version (version operation)
  (unless (and (stringp version)
               (member version '("HTTP/1.0" "HTTP/1.1" "HTTP/2" "HTTP/3")
                       :test #'string=))
    (error 'http-protocol-error
           :message "HTTP protocol version is unsupported."
           :operation operation
           :detail version))
  (copy-seq version))

(defun %validate-body-length (body-length operation)
  (unless (or (null body-length)
              (and (integerp body-length) (>= body-length 0)))
    (error 'http-protocol-error
           :message "HTTP body-length must be a non-negative integer or NIL."
           :operation operation
           :detail body-length))
  body-length)

(defun %status-body-forbidden-p (status request-method)
  (or (and status (<= 100 status 199))
      (member status '(204 304) :test #'=)
      (and request-method
           (string-equal (http-method-name request-method) "HEAD"))))

(defun %connect-tunnel-p (status request-method)
  (and status
       request-method
       (http-method-connect-p request-method)
       (<= 200 status 299)))

(defun %response-status-forbids-framing-field-p (status)
  (and status
       (or (<= 100 status 199)
           (= status 204))))

(defun %response-framing-field-forbidden-p (status request-method)
  (or (%response-status-forbids-framing-field-p status)
      (%connect-tunnel-p status request-method)))

(defun %copy-http-uri (uri)
  (make-http-uri :scheme (http-uri-scheme uri)
                 :authority (http-uri-authority uri)
                 :path (http-uri-path uri)
                 :query (http-uri-query uri)))

(defun %coerce-uri (uri)
  (cond ((http-uri-p uri) (%copy-http-uri uri))
        ((stringp uri) (parse-http-uri uri))
        (t (error 'http-invalid-uri
                  :message "An HTTP request URI must be an HTTP-URI or string."
                  :operation :request
                  :input (type-of uri)))))

(defun %copy-string-or-nil (value)
  (and value (copy-seq value)))

(defun %request-target-value-p (target)
  (and (stringp target)
       (plusp (length target))
       (loop for character across target
             for code = (char-code character)
             always (<= #x21 code #x7e))))

(defun %request-target-form (target)
  (cond ((string= target "*") :asterisk)
        ((char= (char target 0) #\/) :origin)
        ((search "://" target :test #'char-equal) :absolute)
        (t :authority)))

(defun %request-target-error (target detail)
  (error 'http-protocol-error
         :message "HTTP request-target has an invalid form."
         :operation :request
         :detail (list :target target :reason detail)))

(defun %validate-request-target (target)
  (unless (%request-target-value-p target)
    (%request-target-error target :characters))
  (case (%request-target-form target)
    (:asterisk target)
    (:origin
     (let* ((query-position (position #\? target))
            (path (if query-position
                      (subseq target 0 query-position)
                      target))
            (query (and query-position (subseq target (1+ query-position)))))
       (unless (and (plusp (length path)) (char= (char path 0) #\/))
         (%request-target-error target :origin-path))
       (%validate-uri-component path target :path)
       (when query
         (%validate-uri-component query target :query))
       target))
    (:absolute
     (parse-http-uri target)
     target)
    (:authority
     (multiple-value-bind (host port ipv6-p) (%authority-parts target target)
       (declare (ignore host ipv6-p))
       (unless port
         (%request-target-error target :authority-port))
       target))))

(defun %request-target-from-uri (uri)
  (let ((path (http-uri-path uri))
        (query (http-uri-query uri)))
    (if (null query)
        path
        (concatenate 'string path "?" query))))

(defun %wrap-body-function (body-function body-length operation)
  (let ((observed 0)
        (finished-p nil))
    (lambda ()
      (unless finished-p
        (let ((chunk (funcall body-function)))
          (if (null chunk)
              (progn
                (setf finished-p t)
                (when (and body-length (/= observed body-length))
                  (error 'http-protocol-error
                         :message "HTTP body stream ended before its declared length."
                         :operation operation
                         :detail (list :expected body-length :observed observed))))
              (let* ((octets (%copy-octets chunk))
                     (next (+ observed (length octets))))
                (when (and body-length (> next body-length))
                  (error 'http-protocol-error
                         :message "HTTP body stream exceeded its declared length."
                         :operation operation
                         :detail (list :expected body-length :observed next)))
                (setf observed next)
                octets)))))))
