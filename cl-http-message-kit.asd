(in-package #:asdf-user)

(asdf:defsystem "cl-http-message-kit"
  :description "HTTP message values, HTTP/1 wire codecs, and HTTP/2/3 framing and header compression."
  :author "nerima-lisp"
  :license "MIT"
  :version "1.0.0"
  :depends-on ("cl-hpack-codec-kit")
  :pathname "src"
  :serial nil
  :components
  ((:file "package")
   (:file "conditions" :depends-on ("package"))
   (:file "model-declarations" :depends-on ("package"))
   (:file "defaults" :depends-on ("package"))
   (:file "status-registry" :depends-on ("package"))
   (:file "status-reasons" :depends-on ("package" "status-registry"))
   (:file "utilities" :depends-on ("package" "conditions"))
   (:file "methods" :depends-on ("package" "conditions" "utilities"))
   (:file "control-macros" :depends-on ("package" "conditions" "utilities"))
   (:file "uri-data" :depends-on ("package" "conditions" "utilities"))
   (:file "uri-syntax" :depends-on ("package" "conditions" "utilities" "uri-data"))
   (:file "uri" :depends-on ("package" "conditions" "utilities" "uri-data" "uri-syntax"))
   (:file "header-data" :depends-on ("package"))
   (:file "headers" :depends-on ("package" "conditions" "header-data" "utilities"))
   (:file "model-validation"
          :depends-on ("package" "conditions" "utilities" "methods" "uri-data" "uri-syntax" "uri"))
   (:file "request-model"
          :depends-on ("package" "conditions" "model-declarations" "defaults" "utilities"
                       "uri-data" "uri-syntax" "uri" "headers" "model-validation"))
   (:file "response-model"
          :depends-on ("package" "conditions" "model-declarations" "status-reasons" "defaults"
                       "utilities" "uri-data" "uri-syntax" "uri" "headers" "model-validation"))
   (:file "body-cps" :depends-on ("package" "conditions"))
   (:file "model"
          :depends-on ("package" "model-declarations" "utilities" "uri-data" "headers"
                       "request-model" "response-model"))
   (:file "message-semantics"
          :depends-on ("package" "conditions" "utilities" "methods" "headers"
                       "model" "request-model" "response-model"))
   (:file "http1-codec"
          :depends-on ("package" "conditions" "defaults" "utilities" "methods"
                       "headers" "uri" "model-validation" "model" "request-model"
                       "response-model" "message-semantics"))
   (:file "hpack-data" :depends-on ("package"))
   (:file "hpack"
          :depends-on ("package" "conditions" "defaults" "utilities" "headers" "hpack-data"))
   (:file "qpack"
          :depends-on ("package" "conditions" "defaults" "utilities" "headers" "hpack"))
   (:file "http2-3-codec"
          :depends-on ("package" "conditions" "utilities" "methods" "headers" "hpack" "qpack")))
  :in-order-to ((test-op (test-op "cl-http-message-kit/test"))))

(asdf:defsystem "cl-http-message-kit/test"
  :description "Tests for cl-http-message-kit."
  :depends-on ("cl-http-message-kit")
  :pathname "t"
  :serial nil
  :components
  ((:file "package")
   (:file "tests-uri" :depends-on ("package"))
   (:file "tests-headers" :depends-on ("package"))
   (:file "tests-model" :depends-on ("package"))
   (:file "tests-advanced" :depends-on ("package"))
   (:file "tests-contracts" :depends-on ("package"))
   (:file "tests-http2-3" :depends-on ("package"))
   (:file "tests-http1" :depends-on ("package"))
   (:file "tests-qpack" :depends-on ("package"))
   (:file "runner" :depends-on ("package" "tests-uri" "tests-headers" "tests-model"
                                "tests-advanced" "tests-contracts" "tests-http2-3"
                                "tests-http1" "tests-qpack")))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call "HTTP-MESSAGE-KIT/TEST" "RUN-TESTS")))
