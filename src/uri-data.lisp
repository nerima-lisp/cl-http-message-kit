(in-package #:http-message-kit)

(defstruct (http-uri (:constructor %make-http-uri)
                     (:conc-name %uri-))
  scheme
  authority
  host
  port
  path
  query)

(defun %uri-error (input detail)
  (error 'http-invalid-uri
         :message (format nil "Invalid HTTP URI (~A)." detail)
         :operation :uri
         :detail detail
         :input (%bounded-diagnostic input)))

(defun http-uri-scheme (uri)
  (check-type uri http-uri)
  (copy-seq (%uri-scheme uri)))

(defun http-uri-authority (uri)
  (check-type uri http-uri)
  (copy-seq (%uri-authority uri)))

(defun http-uri-host (uri)
  (check-type uri http-uri)
  (copy-seq (%uri-host uri)))

(defun http-uri-port (uri)
  (check-type uri http-uri)
  (%uri-port uri))

(defun http-uri-path (uri)
  (check-type uri http-uri)
  (copy-seq (%uri-path uri)))

(defun http-uri-query (uri)
  (check-type uri http-uri)
  (let ((query (%uri-query uri)))
    (and query (copy-seq query))))
