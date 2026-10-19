(in-package #:http-message-kit)

(defun %default-reason (status)
  (http-status-reason status))

(defun make-http-response
    (&key status reason headers trailers body (protocol-version "HTTP/1.1"))
  (unless (and (integerp status) (<= 100 status 599))
    (error 'http-invalid-status
           :message "HTTP status must be an integer from 100 through 599."
           :operation :response
           :line status
           :code status))
  (when (and reason (not (stringp reason)))
    (error 'http-invalid-status
           :message "HTTP response reason must be a string."
           :operation :response
           :line reason
           :code status))
  (when (and reason (not (%header-value-p reason)))
    (error 'http-invalid-status
           :message "HTTP response reason cannot contain control characters."
           :operation :response
           :line reason
           :code status))
  (let* ((body-octets (if body (%copy-octets body) (%empty-octets)))
         (normalized-trailers (%normalize-headers trailers)))
    (%make-http-response
     :protocol-version (%validate-protocol-version protocol-version :response)
     :status status
     :reason (copy-seq (or reason (%default-reason status)))
     :headers (%normalize-headers headers)
     :trailers normalized-trailers
     :body body-octets)))

(defun http-response-protocol-version (response)
  (check-type response http-response)
  (%response-protocol-version response))

(defun http-response-status (response)
  (check-type response http-response)
  (%response-status response))

(defun http-response-reason (response)
  (check-type response http-response)
  (copy-seq (%response-reason response)))

(defun http-response-headers (response)
  (check-type response http-response)
  (mapcar #'%copy-http-header (%response-headers response)))

(defun http-response-trailers (response)
  (check-type response http-response)
  (mapcar #'%copy-http-header (%response-trailers response)))

(defun http-response-body (response)
  (check-type response http-response)
  (%copy-octets (%response-body response)))

(defun make-http-response-stream
    (&key status reason headers trailers body-function body-length
          (protocol-version "HTTP/1.1"))
  (unless (or (null body-function) (functionp body-function))
    (error 'http-protocol-error
           :message "HTTP response stream body-function must be a function."
           :operation :response
           :detail (type-of body-function)))
  (%validate-body-length body-length :response)
  (let ((response (make-http-response
                   :protocol-version protocol-version
                   :status status
                   :reason reason
                   :headers headers
                   :trailers trailers)))
    (%make-http-response-stream
     :protocol-version (%response-protocol-version response)
     :status (%response-status response)
     :reason (%response-reason response)
     :headers (%response-headers response)
     :trailers (%response-trailers response)
     :body-function body-function
     :body-length body-length)))

(defun http-response-stream-protocol-version (stream)
  (check-type stream http-response-stream)
  (%response-stream-protocol-version stream))

(defun http-response-stream-status (stream)
  (check-type stream http-response-stream)
  (%response-stream-status stream))

(defun http-response-stream-reason (stream)
  (check-type stream http-response-stream)
  (copy-seq (%response-stream-reason stream)))

(defun http-response-stream-headers (stream)
  (check-type stream http-response-stream)
  (mapcar #'%copy-http-header (%response-stream-headers stream)))

(defun http-response-stream-trailers (stream)
  (check-type stream http-response-stream)
  (mapcar #'%copy-http-header (%response-stream-trailers stream)))

(defun http-response-stream-body-function (stream)
  (check-type stream http-response-stream)
  (%response-stream-body-function stream))

(defun http-response-stream-body-length (stream)
  (check-type stream http-response-stream)
  (%response-stream-body-length stream))
