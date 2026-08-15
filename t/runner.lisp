(in-package #:http-message-kit/test)

(defun run-tests ()
  (unless (run-all :reporter :spec :pass-with-no-tests nil)
    (error "cl-http-message-kit tests failed."))
  t)
