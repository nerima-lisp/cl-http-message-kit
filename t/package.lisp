(defpackage #:http-message-kit/test
  (:use #:cl #:http-message-kit)
  (:shadowing-import-from #:cl-weave
                          #:describe)
  (:import-from #:cl-weave
                #:around-each
                #:after-each
                #:before-each
                #:describe-each
                #:expect
                #:gen-integer
                #:it
                #:it-each
                #:it-property
                #:run-all
                #:signals
                #:with-continuation-values)
  (:export #:run-tests))

(in-package #:http-message-kit/test)

(defun octets (&rest values)
  (make-array (length values)
              :element-type '(unsigned-byte 8)
              :initial-contents values))
