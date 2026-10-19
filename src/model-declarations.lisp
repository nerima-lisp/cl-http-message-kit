(in-package #:http-message-kit)

(defstruct (http-request (:constructor %make-http-request)
                         (:conc-name %request-))
  protocol-version
  protocol
  method
  uri
  target
  headers
  trailers
  body)

(defstruct (http-request-stream (:constructor %make-http-request-stream)
                                (:conc-name %request-stream-))
  protocol-version
  method
  uri
  target
  headers
  trailers
  body-function
  body-length)

(defstruct (http-response (:constructor %make-http-response)
                          (:conc-name %response-))
  protocol-version
  status
  reason
  headers
  trailers
  body)

(defstruct (http-response-stream (:constructor %make-http-response-stream)
                                 (:conc-name %response-stream-))
  protocol-version
  status
  reason
  headers
  trailers
  body-function
  body-length)
