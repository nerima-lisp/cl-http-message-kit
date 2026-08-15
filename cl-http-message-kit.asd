(in-package #:asdf-user)

(asdf:defsystem "cl-http-message-kit"
  :description "Validated HTTP message values: URIs, header fields, requests, responses, deadlines and conditions."
  :author "nerima-lisp"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "model-declarations")
               (:file "status-reasons")
               (:file "defaults")
               (:file "utilities")
               (:file "control-macros")
               (:file "uri")
               (:file "header-data")
               (:file "headers")
               (:file "model"))
  :in-order-to ((test-op (test-op "cl-http-message-kit/test"))))

(asdf:defsystem "cl-http-message-kit/test"
  :description "Tests for cl-http-message-kit."
  :depends-on ("cl-http-message-kit" "cl-weave")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "tests-uri")
               (:file "tests-headers")
               (:file "tests-model")
               (:file "runner"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call "HTTP-MESSAGE-KIT/TEST" "RUN-TESTS")))
