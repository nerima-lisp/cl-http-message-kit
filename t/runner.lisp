(in-package #:http-message-kit/test)

(defparameter *test-timeout-ms* 30000)

(defun run-tests (&key coverage)
  (unless (run-all :reporter :spec
                   :pass-with-no-tests nil
                   :timeout-ms *test-timeout-ms*
                   :max-workers 1
                   :coverage coverage)
    (error "cl-http-message-kit tests failed."))
  t)
