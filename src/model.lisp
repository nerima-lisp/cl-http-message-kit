(in-package #:http-message-kit)

(defmethod print-object ((request http-request) stream)
  (print-unreadable-object (request stream :type t)
    (let ((uri (%request-uri request)))
      (format stream "~A ~A://~A~A~A headers=~D body-bytes=~D"
              (%request-method request)
              (http-uri-scheme uri)
              (http-uri-authority uri)
              (http-uri-path uri)
              (if (http-uri-query uri) "?<redacted>" "")
              (length (%request-headers request))
              (length (%request-body request))))))

(defmethod print-object ((response http-response) stream)
  (print-unreadable-object (response stream :type t)
    (format stream "~A status=~D headers=~D trailers=~D body-bytes=~D"
            (%response-protocol-version response)
            (%response-status response)
            (length (%response-headers response))
            (length (%response-trailers response))
            (length (%response-body response)))))
