(in-package #:http-message-kit)

(defun http-body-cps (body-function on-chunk on-end &key on-error)
  "Consume BODY-FUNCTION through continuation callbacks.
ON-CHUNK receives each non-NIL chunk and a continuation.  A NIL chunk
invokes ON-END.  ON-ERROR receives a condition signaled by BODY-FUNCTION;
when absent, that condition is re-signaled."
  (flet ((require-function (function operation)
           (unless (functionp function)
             (error 'http-protocol-error
                    :message "HTTP body CPS callbacks must be functions."
                    :operation operation
                    :detail (type-of function)))))
    (require-function body-function :body)
    (require-function on-chunk :on-chunk)
    (require-function on-end :on-end)
    (when on-error
      (require-function on-error :on-error)))
  (labels ((read-next ()
             (handler-case
                 (let ((chunk (funcall body-function)))
                   (values t (and chunk (%copy-octets chunk))))
               (error (condition)
                 (if on-error
                     (values nil condition)
                     (error condition)))))
           (continue-reading ()
             (multiple-value-bind (read-p chunk) (read-next)
               (if read-p
                   (if chunk
                       (funcall on-chunk chunk #'continue-reading)
                       (funcall on-end))
                   (funcall on-error chunk)))))
    (continue-reading)))
