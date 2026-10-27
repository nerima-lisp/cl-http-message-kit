(defpackage #:http-message-kit/test-support
            (:use #:cl)
            (:export #:make-qpack-first-dynamic-entry-identity-probe
                     #:call-with-http3-qpack-decoded-callback-observer))
