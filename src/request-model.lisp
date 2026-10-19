(in-package #:http-message-kit)

(defun make-http-request
    (&key method protocol uri request-target headers trailers body
          (protocol-version "HTTP/1.1"))
  (unless (and (stringp method) (%token-p method))
    (error 'http-protocol-error
           :message "HTTP request methods must be non-empty tokens."
           :operation :request
           :detail method))
  (when (and protocol
             (not (and (stringp protocol) (%token-p protocol))))
    (error 'http-protocol-error
           :message "HTTP request protocols must be non-empty tokens."
           :operation :request
           :detail protocol))
  (when request-target
    (unless (%request-target-value-p request-target)
      (%request-target-error request-target :characters)))
  (let* ((normalized-trailers
           (let ((normalized (%normalize-headers trailers)))
             (loop for source in trailers
                   for trailer in normalized
                   for name = (if (http-header-p source)
                                  (http-header-name source)
                                  (car source))
                   collect (make-http-header
                            name
                            (http-header-content trailer)))))
         (request
           (%make-http-request
            :protocol-version (%validate-protocol-version protocol-version :request)
            :protocol (and protocol (%ascii-lowercase protocol))
            :method (copy-seq method)
            :uri (%coerce-uri uri)
            :target (%copy-string-or-nil request-target)
            :headers (%normalize-headers headers :copy-name-p nil)
            :trailers normalized-trailers
            :body (if body (%copy-octets body) (%empty-octets)))))
    (when (and (string= method "TRACE")
               (plusp (length (%request-body request))))
      (error 'http-protocol-error
             :message "TRACE requests must not contain content."
             :operation :request
             :detail :trace-content))
    request))

(defun http-request-protocol-version (request)
  (check-type request http-request)
  (%request-protocol-version request))

(defun http-request-protocol (request)
  (check-type request http-request)
  (%copy-string-or-nil (%request-protocol request)))

(defun http-request-method (request)
  (check-type request http-request)
  (copy-seq (%request-method request)))

(defun http-request-uri (request)
  (check-type request http-request)
  (%copy-http-uri (%request-uri request)))

(defun http-request-headers (request)
  (check-type request http-request)
  (mapcar #'%copy-http-header (%request-headers request)))

(defun http-request-body (request)
  (check-type request http-request)
  (%copy-octets (%request-body request)))

(defun http-request-target (request)
  (check-type request http-request)
  (or (%copy-string-or-nil (%request-target request))
      (%request-target-from-uri (%request-uri request))))

(defun http-request-target-form (request)
  (let ((target (http-request-target request)))
    (%request-target-form target)))

(defun http-request-trailers (request)
  (check-type request http-request)
  (mapcar #'%copy-http-header (%request-trailers request)))

(defun http-request-authority (request)
  (check-type request http-request)
  (http-uri-authority (%request-uri request)))

(defun http-request-path (request)
  (check-type request http-request)
  (http-uri-path (%request-uri request)))

(defun http-request-query (request)
  (check-type request http-request)
  (http-uri-query (%request-uri request)))

(defun make-http-request-stream
    (&key method uri request-target headers trailers body-function body-length
          (protocol-version "HTTP/1.1"))
  (unless (functionp body-function)
    (error 'http-protocol-error
           :message "HTTP request stream body-function must be a function."
           :operation :request
           :detail (type-of body-function)))
  (%validate-body-length body-length :request)
  (let ((request (make-http-request
                  :protocol-version protocol-version
                  :method method
                  :uri uri
                  :request-target request-target
                  :headers headers
                  :trailers trailers)))
    (%make-http-request-stream
     :protocol-version (%request-protocol-version request)
     :method (%request-method request)
     :uri (%request-uri request)
     :target (%request-target request)
     :headers (%request-headers request)
     :trailers (%request-trailers request)
     :body-function (%wrap-body-function body-function body-length :request)
     :body-length body-length)))

(defun http-request-stream-protocol-version (stream)
  (check-type stream http-request-stream)
  (%request-stream-protocol-version stream))

(defun http-request-stream-method (stream)
  (check-type stream http-request-stream)
  (copy-seq (%request-stream-method stream)))

(defun http-request-stream-uri (stream)
  (check-type stream http-request-stream)
  (%copy-http-uri (%request-stream-uri stream)))

(defun http-request-stream-target (stream)
  (check-type stream http-request-stream)
  (or (%copy-string-or-nil (%request-stream-target stream))
      (%request-target-from-uri (%request-stream-uri stream))))

(defun http-request-stream-target-form (stream)
  (let ((target (http-request-stream-target stream)))
    (%request-target-form target)))

(defun http-request-stream-headers (stream)
  (check-type stream http-request-stream)
  (mapcar #'%copy-http-header (%request-stream-headers stream)))

(defun http-request-stream-trailers (stream)
  (check-type stream http-request-stream)
  (mapcar #'%copy-http-header (%request-stream-trailers stream)))

(defun http-request-stream-body-function (stream)
  (check-type stream http-request-stream)
  (%request-stream-body-function stream))

(defun http-request-stream-body-length (stream)
  (check-type stream http-request-stream)
  (%request-stream-body-length stream))
