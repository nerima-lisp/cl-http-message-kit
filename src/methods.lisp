(in-package #:http-message-kit)

(defparameter *http-method-registry*
  '(("CONNECT" :safe nil :idempotent nil :cacheable nil)
    ("DELETE" :safe nil :idempotent t :cacheable nil)
    ("GET" :safe t :idempotent t :cacheable t)
    ("HEAD" :safe t :idempotent t :cacheable t)
    ("OPTIONS" :safe t :idempotent t :cacheable nil)
    ("PATCH" :safe nil :idempotent nil :cacheable nil)
    ("POST" :safe nil :idempotent nil :cacheable nil)
    ("PUT" :safe nil :idempotent t :cacheable nil)
    ("TRACE" :safe t :idempotent t :cacheable nil)))

(defun %method-designator-string (method)
  (typecase method
    (string method)
    (symbol (symbol-name method))
    (t (error 'http-protocol-error
              :message "HTTP methods must be strings or symbols."
              :operation :method
              :detail (type-of method)))))

(defun http-method-p (method)
  (let ((name (%method-designator-string method)))
    (%token-p name)))

(defun http-method-name (method)
  (let ((name (%method-designator-string method)))
    (unless (%token-p name)
      (error 'http-protocol-error
             :message "HTTP methods must be non-empty tokens."
             :operation :method
             :detail method))
    (string-upcase name)))

(defun %method-properties (method)
  (cdr (assoc (http-method-name method)
              *http-method-registry*
              :test #'string=)))

(defun http-method-known-p (method)
  (not (null (%method-properties method))))

(defun http-method-properties (method)
  (copy-list (%method-properties method)))

(defun %method-property-p (method property)
  (getf (%method-properties method) property))

(defun http-method-safe-p (method)
  (%method-property-p method :safe))

(defun http-method-idempotent-p (method)
  (%method-property-p method :idempotent))

(defun http-method-cacheable-p (method)
  (%method-property-p method :cacheable))

(defun http-method-connect-p (method)
  (string= (http-method-name method) "CONNECT"))
