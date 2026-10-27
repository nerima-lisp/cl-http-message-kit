(in-package #:http-message-kit/test-support)

(defun make-qpack-first-dynamic-entry-identity-probe (context) "Return a predicate for whether CONTEXT keeps its current first dynamic entry object."
  (let* ((entry
           (first
            (http-message-kit::qpack-dynamic-table-entries
             (http-message-kit::qpack-context-dynamic-table context)))))
    (unless entry
      (error "QPACK dynamic table has no first entry to observe."))
    (lambda ()
      (eq entry
          (first
           (http-message-kit::qpack-dynamic-table-entries
            (http-message-kit::qpack-context-dynamic-table context)))))))

(defun call-with-http3-qpack-decoded-callback-observer
    (thunk &key on-wired on-decoded)
  "Observe HTTP/3 QPACK callback wiring while THUNK runs. Use only while decoder calls are serialized because the instrumented function cell is process-wide."
  (unless (functionp thunk)
    (error "THUNK must be a function."))
  (unless (or (null on-wired) (functionp on-wired))
    (error "ON-WIRED must be a function or NIL."))
  (unless (or (null on-decoded) (functionp on-decoded))
    (error "ON-DECODED must be a function or NIL."))
  (let* ((decoder-symbol
           (quote http-message-kit::decode-http3-qpack-field-section))
         (original-decoder (symbol-function decoder-symbol))
         (wrapped-callbacks (make-hash-table :test 'eq)))
    (unwind-protect
         (progn
           (setf (symbol-function decoder-symbol)
                 (lambda (octets context stream-id &rest arguments)
                   (let ((callback (getf arguments :on-decoded)))
                     (when (and (functionp callback)
                                (not (gethash callback wrapped-callbacks)))
                       (when on-wired
                         (funcall on-wired callback))
                       (let ((wrapped-callback
                               (lambda (fields)
                                 (when on-decoded
                                   (funcall on-decoded fields))
                                 (funcall callback fields))))
                         (setf (gethash wrapped-callback wrapped-callbacks) t
                               (getf arguments :on-decoded) wrapped-callback)))
                     (apply original-decoder octets context stream-id
                            arguments))))
           (funcall thunk))
      (setf (symbol-function decoder-symbol) original-decoder))))
