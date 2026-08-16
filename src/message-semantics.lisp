(in-package #:http-message-kit)

(defstruct (http-message-framing (:constructor %make-http-message-framing)
                                 (:conc-name %framing-))
  kind
  length
  transfer-codings
  body-permitted-p
  request-p
  status
  request-method)

(defun http-message-framing-kind (framing)
  (check-type framing http-message-framing)
  (%framing-kind framing))

(defun http-message-framing-length (framing)
  (check-type framing http-message-framing)
  (%framing-length framing))

(defun http-message-framing-transfer-codings (framing)
  (check-type framing http-message-framing)
  (copy-list (%framing-transfer-codings framing)))

(defun http-message-framing-body-permitted-p (framing)
  (check-type framing http-message-framing)
  (%framing-body-permitted-p framing))

(defun %framing-error (detail)
  (error 'http-invalid-framing
         :message "HTTP message framing fields are inconsistent."
         :operation :framing
         :detail detail
         :framing detail))

(defun %header-list-items (values &key (allow-empty nil))
  (loop for value in values
        append
        (let ((start 0)
              (length (length value))
              (items nil))
          (loop for comma = (position #\, value :start start)
                for end = (or comma length)
                for item = (%trim-ows (subseq value start end))
                do (if (plusp (length item))
                       (push item items)
                       (unless allow-empty
                         (%framing-error (list :empty-list-item value))))
                   (if comma
                       (setf start (1+ comma))
                       (return (nreverse items)))))))

(defun %content-length-values (headers)
  (let ((values (http-header-values headers "content-length")))
    (when values
      (let ((lengths
              (mapcar (lambda (value)
                        (unless (%decimal-string-p value)
                          (%framing-error (list :content-length value)))
                        (%parse-decimal value))
                      (%header-list-items values))))
        (unless (apply #'= lengths)
          (%framing-error (list :conflicting-content-length lengths)))
        (first lengths)))))

(defun %transfer-parameter-value-p (value)
  (or (%token-p value)
      (and (>= (length value) 2)
           (char= (char value 0) #\")
           (char= (char value (1- (length value))) #\"))))

(defun %validate-transfer-coding-parameters (parameters item)
  (loop with start = 0
        for separator = (position #\; parameters :start start)
        for end = (or separator (length parameters))
        for parameter = (%trim-ows (subseq parameters start end))
        do (progn
             (let ((equals (position #\= parameter)))
               (unless (and equals
                            (plusp equals)
                            (< (1+ equals) (length parameter))
                            (%token-p (%trim-ows (subseq parameter 0 equals)))
                            (%transfer-parameter-value-p
                             (%trim-ows (subseq parameter (1+ equals)))))
                 (%framing-error
                  (list :transfer-encoding-parameter item))))
             (if separator
                 (setf start (1+ separator))
                 (return)))))

(defun %transfer-coding-values (headers)
  (let ((values (http-header-values headers "transfer-encoding")))
    (when values
      (mapcar (lambda (item)
                (let* ((semicolon (position #\; item))
                       (coding (%trim-ows (if semicolon
                                             (subseq item 0 semicolon)
                                             item)))
                       (parameters (and semicolon
                                        (subseq item (1+ semicolon)))))
                  (unless (%token-p coding)
                    (%framing-error (list :transfer-encoding item)))
                  (when parameters
                    (%validate-transfer-coding-parameters parameters item))
                  (%ascii-lowercase coding)))
              (%header-list-items values)))))

(defun http-response-body-allowed-p (status &optional request-method)
  (not (or (%status-body-forbidden-p status request-method)
           (%connect-tunnel-p status request-method))))

(defun %make-framing (&key request-p headers status request-method)
  (let* ((content-length (%content-length-values headers))
         (transfer-codings (%transfer-coding-values headers))
         (body-forbidden-p (and (not request-p)
                                (or (%status-body-forbidden-p status request-method)
                                    (%connect-tunnel-p status request-method))))
         (tunnel-p (and (not request-p)
                        (%connect-tunnel-p status request-method))))
    (when (and content-length transfer-codings)
      (%framing-error :content-length-with-transfer-encoding))
    (when (and request-p transfer-codings
               (not (string= (car (last transfer-codings)) "chunked")))
      (%framing-error (list :request-transfer-encoding-not-final
                            transfer-codings)))
    (when (and transfer-codings
               (> (count "chunked" transfer-codings :test #'string=) 1))
      (%framing-error (list :duplicate-chunked transfer-codings)))
    (when (and (not request-p)
               content-length
               (%response-status-forbids-framing-field-p status))
      (%framing-error :content-length-on-bodyless-response))
    (when (and (not request-p)
               transfer-codings
               (%response-status-forbids-framing-field-p status))
      (%framing-error :transfer-encoding-on-bodyless-response))
    (cond
      (tunnel-p
       (when transfer-codings
         (%framing-error :transfer-encoding-on-connect-tunnel))
       (%make-http-message-framing
        :kind :tunnel
        :length nil
        :transfer-codings nil
        :body-permitted-p nil
        :request-p request-p
        :status status
        :request-method request-method))
      (body-forbidden-p
       (%make-http-message-framing
        :kind :none
        :length 0
        :transfer-codings transfer-codings
        :body-permitted-p nil
        :request-p request-p
        :status status
        :request-method request-method))
      (transfer-codings
       (if (and (string= (car (last transfer-codings)) "chunked")
                (or request-p (member "chunked" transfer-codings :test #'string=)))
           (%make-http-message-framing
            :kind :chunked
            :length nil
            :transfer-codings transfer-codings
            :body-permitted-p t
            :request-p request-p
            :status status
            :request-method request-method)
           (%make-http-message-framing
            :kind :close-delimited
            :length nil
            :transfer-codings transfer-codings
            :body-permitted-p t
            :request-p request-p
            :status status
            :request-method request-method)))
      (content-length
       (%make-http-message-framing
        :kind :content-length
        :length content-length
        :transfer-codings nil
        :body-permitted-p t
        :request-p request-p
        :status status
        :request-method request-method))
      (request-p
       (%make-http-message-framing
        :kind :none
        :length 0
        :transfer-codings nil
        :body-permitted-p nil
        :request-p t
        :status status
        :request-method request-method))
      (t
       (%make-http-message-framing
        :kind :close-delimited
        :length nil
        :transfer-codings nil
        :body-permitted-p t
        :request-p nil
        :status status
        :request-method request-method)))))

(defun http-request-framing (request)
  (check-type request http-request)
  (%make-framing :request-p t
                 :headers (http-request-headers request)
                 :request-method (http-request-method request)))

(defun http-response-framing (response &key request-method)
  (check-type response http-response)
  (%make-framing :headers (http-response-headers response)
                 :status (http-response-status response)
                 :request-method request-method))

(defun http-message-framing (message &key request-method)
  (typecase message
    (http-request (http-request-framing message))
    (http-response (http-response-framing message :request-method request-method))
    (t (error 'http-protocol-error
              :message "Expected an HTTP request or response message."
              :operation :framing
              :detail (type-of message)))))

(defun http-message-body-length (message &key request-method)
  (let ((framing (http-message-framing message :request-method request-method)))
    (http-message-framing-length framing)))

(defun http-message-body-allowed-p (message &key request-method)
  (http-message-framing-body-permitted-p
   (http-message-framing message :request-method request-method)))
