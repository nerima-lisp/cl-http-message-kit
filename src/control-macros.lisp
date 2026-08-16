(in-package #:http-message-kit)

(defmacro with-http-deadline
    ((deadline timeout &key inherited
                           (clock-function #'%monotonic-time)
                           (kind :connect))
     &body body)
  (let ((clock (gensym "CLOCK-FUNCTION-")))
    `(let* ((,clock ,clock-function)
            (,deadline
              (http-deadline ,timeout
                             :deadline ,inherited
                             :clock-function ,clock)))
       (%check-deadline ,deadline ,clock ,kind)
       ,@body)))
