(defpackage #:http-message-kit/test
  (:use #:cl #:http-message-kit)
  (:shadowing-import-from #:cl-weave
                          #:describe)
  (:import-from #:cl-weave
                #:expect
                #:it
                #:run-all
                #:signals)
  (:export #:run-tests))

(in-package #:http-message-kit/test)

(defun octets (&rest values)
  (make-array (length values)
              :element-type '(unsigned-byte 8)
              :initial-contents values))
