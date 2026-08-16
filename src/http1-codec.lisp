(in-package #:http-message-kit)

(defun %http1-incomplete (needed available)
  (error 'http-incomplete-message
         :message "The HTTP/1 message ended before its framing was complete."
         :operation :http1-parse
         :needed needed
         :available available
         :detail (list :needed needed :available available)))

(defun %crlf-octets ()
  (%octets-from-string
   (coerce (list #\return #\linefeed) 'string)
   :ascii-only t))

(defun %octets-subseq (octets start &optional end)
  (let ((result (make-array (- (or end (length octets)) start)
                            :element-type '(unsigned-byte 8))))
    (replace result octets :start2 start :end2 (or end (length octets)))
    result))

(defun %find-crlf (octets start)
  (loop for index from start below (1- (length octets))
        when (and (= (aref octets index) #x0d)
                  (= (aref octets (1+ index)) #x0a))
          do (return index)))

(defun %wire-line-string (octets start end)
  (let ((line (%string-from-octets (%octets-subseq octets start end))))
    (when (or (find #\return line) (find #\linefeed line))
      (error 'http-protocol-error
             :message "HTTP/1 lines cannot contain bare CR or LF."
             :operation :http1-parse
             :detail :bare-line-break))
    line))

(defun %parse-wire-header-line (line)
  (when (and (plusp (length line))
             (member (char line 0) '(#\Space #\Tab) :test #'char=))
    (error 'http-protocol-error
           :message "HTTP obsolete line folding is not accepted."
           :operation :http1-parse
           :detail line))
  (let ((colon (position #\: line)))
    (unless (and colon (plusp colon))
      (error 'http-invalid-header
             :message "HTTP/1 header lines require a field name and colon."
             :operation :http1-parse
             :name line
             :reason :syntax))
    (let ((name (subseq line 0 colon))
          (content (subseq line (1+ colon))))
      (unless (%header-name-p name)
        (error 'http-invalid-header
               :message "HTTP/1 header field names must be ASCII tokens."
               :operation :http1-parse
               :name name
               :reason :name))
      (make-http-header name (%trim-ows content)))))

(defun %parse-http1-header-block (octets start max-header-bytes)
  (let ((line-end (%find-crlf octets start)))
    (unless line-end
      (%http1-incomplete :start-line (length octets)))
    (let ((start-line (%wire-line-string octets start line-end))
          (headers nil)
          (position (+ line-end 2)))
      (loop
        (let ((end (%find-crlf octets position)))
          (unless end
            (%http1-incomplete :header-line (length octets)))
          (let ((line (%wire-line-string octets position end)))
            (setf position (+ end 2))
            (http-check-size-limit position max-header-bytes :headers)
            (if (zerop (length line))
                (return (values start-line (nreverse headers) position))
                (push (%parse-wire-header-line line) headers))))))))

(defun %parse-request-line (line)
  (let* ((first-space (position #\Space line))
         (second-space (and first-space
                           (position #\Space line :start (1+ first-space)))))
    (unless (and first-space second-space
                 (plusp first-space)
                 (> second-space (1+ first-space))
                 (null (position #\Space line :start (1+ second-space))))
      (error 'http-protocol-error
             :message "HTTP request-lines require METHOD SP target SP version."
             :operation :http1-parse
             :detail line))
    (let ((method (subseq line 0 first-space))
          (target (subseq line (1+ first-space) second-space))
          (version (subseq line (1+ second-space))))
      (unless (and (%token-p method)
                   (%request-target-value-p target))
        (error 'http-protocol-error
               :message "HTTP request-line method or target is invalid."
               :operation :http1-parse
               :detail line))
      (values method target (%validate-protocol-version version :request)))))

(defun %parse-response-line (line)
  (let ((first-space (position #\Space line)))
    (unless (and first-space
                 (>= (length line) 5)
                 (string= "HTTP/" line :end2 5)
                 (plusp first-space))
      (error 'http-invalid-status
             :message "HTTP status-lines require an HTTP version and status code."
             :operation :http1-parse
             :line line))
    (let* ((version (subseq line 0 first-space))
           (after-version (1+ first-space))
           (second-space (position #\Space line :start after-version))
           (status-text (if second-space
                            (subseq line after-version second-space)
                            (subseq line after-version)))
           (reason (if second-space
                       (subseq line (1+ second-space))
                       "")))
      (unless (and (= (length status-text) 3)
                   (%decimal-string-p status-text))
        (error 'http-invalid-status
               :message "HTTP status-lines require a three-digit status code."
               :operation :http1-parse
               :line line))
      (let ((status (%parse-decimal status-text)))
        (unless (<= 100 status 599)
          (error 'http-invalid-status
                 :message "HTTP status code is outside the supported range."
                 :operation :http1-parse
                 :line line
                 :code status))
        (values (%validate-protocol-version version :response)
                status
                reason)))))

(defun %parse-hex-size (text max-body-bytes)
  (unless (plusp (length text))
    (%framing-error (list :empty-chunk-size text)))
  (let ((value 0))
    (loop for character across text
          for digit = (cond ((digit-char-p character 16) (digit-char-p character 16))
                            (t nil))
          do (unless digit
               (%framing-error (list :invalid-chunk-size text)))
             (setf value (+ (* value 16) digit))
             (when max-body-bytes
               (http-check-size-limit value max-body-bytes :body)))
    value))

(defun %quoted-wire-value-p (value)
  (and (>= (length value) 2)
       (char= (char value 0) #\")
       (char= (char value (1- (length value))) #\")
       (loop with escaped-p = nil
             for index from 1 below (1- (length value))
             for character = (char value index)
             do (cond (escaped-p
                       (unless (or (= (char-code character) #x09)
                                   (<= #x20 (char-code character) #x7e)
                                   (<= #x80 (char-code character) #xff))
                         (return nil))
                       (setf escaped-p nil))
                      ((char= character #\\) (setf escaped-p t))
                      ((or (char= character #\return)
                           (char= character #\linefeed))
                       (return nil)))
             finally (return (not escaped-p)))))

(defun %validate-chunk-extensions (text)
  (when text
    (let ((start 0)
          (length (length text)))
      (loop
        (let* ((semicolon (position #\; text :start start))
               (extension (%trim-ows
                           (subseq text start (or semicolon length))))
               (equals (position #\= extension)))
          (unless (and (plusp (length extension))
                       (if equals
                           (and (plusp equals)
                                (%token-p (%trim-ows (subseq extension 0 equals)))
                                (let ((value (%trim-ows
                                              (subseq extension (1+ equals)))))
                                  (or (%token-p value)
                                      (%quoted-wire-value-p value))))
                           (%token-p extension)))
            (%framing-error (list :invalid-chunk-extension extension)))
          (if semicolon
              (setf start (1+ semicolon))
              (return)))))))

(defun %parse-chunk-size-line (line)
  (let ((semicolon (position #\; line)))
    (let ((size-text (%trim-ows (if semicolon
                                   (subseq line 0 semicolon)
                                   line))))
      (%validate-chunk-extensions (and semicolon (subseq line (1+ semicolon))))
      (%parse-hex-size size-text nil))))

(defun %parse-chunked-at (octets start max-body-bytes max-trailer-bytes)
  (let ((position start)
        (chunks nil)
        (total 0)
        (trailers nil))
    (loop
      (let ((line-end (%find-crlf octets position)))
        (unless line-end
          (%http1-incomplete :chunk-size-line (length octets)))
          (let ((line (%wire-line-string octets position line-end)))
            (setf position (+ line-end 2))
            (let ((size (%parse-chunk-size-line line)))
              (if (zerop size)
                  (let ((trailer-bytes 0))
                    (loop
                      (let ((trailer-start position)
                            (trailer-end (%find-crlf octets position)))
                        (unless trailer-end
                          (%http1-incomplete :trailer-line (length octets)))
                        (let ((trailer-line
                                (%wire-line-string octets position trailer-end)))
                          (setf position (+ trailer-end 2))
                          (incf trailer-bytes (- position trailer-start))
                          (http-check-size-limit trailer-bytes
                                                 max-trailer-bytes
                                                 :trailers)
                          (if (zerop (length trailer-line))
                              (return-from %parse-chunked-at
                                (values (if chunks
                                            (apply #'%append-octets (nreverse chunks))
                                            (%empty-octets))
                                        (http-validate-trailers (nreverse trailers))
                                        position))
                              (push (%parse-wire-header-line trailer-line)
                                    trailers)))))))
                  (progn
                    (when (> size (- (length octets) position))
                      (%http1-incomplete (+ position size)
                                         (length octets)))
                    (incf total size)
                    (http-check-size-limit total max-body-bytes :body)
                    (let ((data-end (+ position size)))
                      (when (> (+ data-end 2) (length octets))
                        (%http1-incomplete :chunk-data-crlf (length octets)))
                      (unless (and (= (aref octets data-end) #x0d)
                                   (= (aref octets (1+ data-end)) #x0a))
                        (%framing-error :missing-chunk-data-crlf))
                      (push (%octets-subseq octets position data-end) chunks)
                      (setf position (+ data-end 2))))))))))

(defun http1-decode-chunked (octets &key (max-body-bytes *default-max-body-bytes*)
                                      (max-trailer-bytes *default-max-header-bytes*))
  "Decode a complete chunked body.  Return BODY, TRAILERS, and octets consumed."
  (let ((copy (%copy-octets octets)))
    (%parse-chunked-at copy 0 max-body-bytes max-trailer-bytes)))

(defun http1-encode-chunked (body &key chunk-size trailers)
  "Encode BODY with HTTP/1 chunk framing and optional TRAILERS."
  (let* ((octets (%copy-octets body))
         (size (or chunk-size (max 1 (length octets)))))
    (unless (and (integerp size) (plusp size))
      (error 'http-protocol-error
             :message "HTTP chunk-size must be a positive integer."
             :operation :http1-encode
             :detail size))
    (let ((pieces nil))
      (loop for start from 0 below (length octets) by size
            for end = (min (length octets) (+ start size))
            do (push (%octets-from-string
                      (format nil "~X~C~C" (- end start) #\return #\linefeed)
                      :ascii-only t)
                     pieces)
               (push (%octets-subseq octets start end) pieces)
               (push (%crlf-octets) pieces))
      (let ((normalized-trailers (http-validate-trailers trailers)))
        (push (%octets-from-string
               (format nil "0~C~C" #\return #\linefeed)
               :ascii-only t)
              pieces)
        (dolist (header (reverse normalized-trailers))
          (push (%octets-from-string
                 (format nil "~A: ~A~C~C"
                         (http-header-name header)
                         (http-header-content header)
                         #\return #\linefeed)
                 :ascii-only nil)
                pieces))
        (push (%crlf-octets) pieces))
      (apply #'%append-octets (nreverse pieces)))))

(defun %header-wire-octets (header)
  (when (http-pseudo-header-p header)
    (error 'http-protocol-error
           :message "HTTP/1 cannot serialize pseudo-header fields."
           :operation :http1-encode
           :detail (http-header-name header)))
  (%append-octets
   (%octets-from-string (http-header-name header) :ascii-only t)
   (%octets-from-string ": " :ascii-only t)
   (%octets-from-string (http-header-content header))
   (%crlf-octets)))

(defun %headers-wire-octets (headers)
  (apply #'%append-octets (mapcar #'%header-wire-octets headers)))

(defun %header-present-or-add (headers name content)
  (if (http-header-present-p headers name)
      headers
      (append headers (list (make-http-header name content)))))

(defun %validate-http1-host-header (headers request-p protocol-version)
  (let* ((values (http-header-values headers "host"))
         (count (length values)))
    (when (> count 1)
      (%framing-error :multiple-host))
    (when (and request-p
               (string= protocol-version "HTTP/1.1")
               (/= count 1))
      (%framing-error :missing-host))
    (when (= count 1)
      (handler-case
          (make-http-uri :authority (first values))
        (http-invalid-uri ()
          (error 'http-protocol-error
                 :message "HTTP Host field is not a valid authority."
                 :operation :http1-host
                 :detail (first values)))))))

(defun %http1-effective-headers (headers body trailers request-p protocol-version
                                  &optional request-method status)
  (let* ((normalized (%normalize-headers headers))
         (body-length (length body))
         (has-trailers (plusp (length trailers)))
         (transfer-values (http-header-values normalized "transfer-encoding"))
         (content-length (%content-length-values normalized))
         (forbidden-p (and (not request-p)
                           (or (%status-body-forbidden-p status request-method)
                               (%connect-tunnel-p status request-method))))
         (tunnel-p (and (not request-p)
                        (%connect-tunnel-p status request-method))))
    (when (and (not request-p)
               (%response-framing-field-forbidden-p status request-method)
               content-length)
      (%framing-error :content-length-on-bodyless-response))
    (when (and (not request-p)
               (%response-framing-field-forbidden-p status request-method)
               transfer-values)
      (%framing-error :transfer-encoding-on-bodyless-response))
    (when (and tunnel-p (plusp body-length))
      (%framing-error :body-on-connect-tunnel))
    (when (and (not request-p) forbidden-p has-trailers)
      (%framing-error :trailers-on-bodyless-response))
    (when (and (not (string= protocol-version "HTTP/1.1"))
               transfer-values)
      (%framing-error :transfer-encoding-requires-http-1-1))
    (when (and content-length (/= content-length body-length)
               (not forbidden-p))
      (%framing-error (list :content-length-mismatch content-length body-length)))
    (when (and (or has-trailers transfer-values)
                 (not forbidden-p)
                 (or (null transfer-values)
                   (not (equal (%transfer-coding-values normalized) '("chunked")))))
      (error 'http-unsupported-feature
             :message "Only a final HTTP/1 chunked transfer-coding is supported."
             :operation :http1-encode
             :feature :transfer-coding))
    (let ((result normalized))
      (when (and (not (string= protocol-version "HTTP/1.0"))
                 request-p
                 (not (http-header-present-p result "host")))
        (setf result (%header-present-or-add
                      result "host"
                      (http-request-authority request-method))))
      (when request-p
        (%validate-http1-host-header result t protocol-version))
      (when (and (not forbidden-p)
                 (or has-trailers transfer-values))
        (unless transfer-values
          (setf result (%header-present-or-add result "transfer-encoding" "chunked"))))
      (when (and (not forbidden-p)
                 (null transfer-values)
                 (null content-length)
                 (not has-trailers)
                 (or (not request-p) (plusp body-length)))
        (setf result (%header-present-or-add result "content-length"
                                             (princ-to-string body-length))))
      result)))

(defun %http1-body-octets (headers body trailers request-p protocol-version
                            &optional request-method status)
  (declare (ignore protocol-version))
  (let* ((transfer-values (%transfer-coding-values headers))
         (forbidden-p (and (not request-p)
                           (or (%status-body-forbidden-p status request-method)
                               (%connect-tunnel-p status request-method)))))
    (cond
      (forbidden-p (%empty-octets))
      ((and transfer-values
            (equal transfer-values '("chunked")))
       (http1-encode-chunked body :trailers trailers))
      (t body))))

(defun http1-serialize-request (request)
  "Serialize an HTTP/1.0 or HTTP/1.1 request to an octet vector."
  (check-type request http-request)
  (let ((version (http-request-protocol-version request)))
    (unless (member version '("HTTP/1.0" "HTTP/1.1") :test #'string=)
      (error 'http-unsupported-feature
             :message "HTTP/1 serialization requires an HTTP/1 protocol version."
             :operation :http1-encode
             :feature version))
    (let* ((body (http-request-body request))
           (trailers (http-request-trailers request))
           (headers (%http1-effective-headers
                     (http-request-headers request)
                     body trailers t version request))
           (wire-body (%http1-body-octets headers body trailers t version request)))
      (%append-octets
       (%octets-from-string
        (format nil "~A ~A ~A~C~C"
                (http-request-method request)
                (http-request-target request)
                version #\return #\linefeed)
        :ascii-only t)
       (%headers-wire-octets headers)
       (%crlf-octets)
       wire-body))))

(defun http1-serialize-response (response &key request-method)
  "Serialize an HTTP/1.0 or HTTP/1.1 response to an octet vector."
  (check-type response http-response)
  (let ((version (http-response-protocol-version response)))
    (unless (member version '("HTTP/1.0" "HTTP/1.1") :test #'string=)
      (error 'http-unsupported-feature
             :message "HTTP/1 serialization requires an HTTP/1 protocol version."
             :operation :http1-encode
             :feature version))
    (let* ((body (http-response-body response))
           (trailers (http-response-trailers response))
           (headers (%http1-effective-headers
                     (http-response-headers response)
                     body trailers nil version request-method
                     (http-response-status response)))
           (wire-body (%http1-body-octets headers body trailers nil version
                                          request-method
                                          (http-response-status response))))
      (%append-octets
       (%octets-from-string
        (format nil "~A ~3,'0D ~A~C~C"
                version
                (http-response-status response)
                (http-response-reason response)
                #\return #\linefeed)
        :ascii-only t)
       (%headers-wire-octets headers)
       (%crlf-octets)
       wire-body))))

(defun %request-uri-from-target (target headers scheme)
  (case (%request-target-form target)
    (:absolute (parse-http-uri target))
    (:origin
     (let* ((host-values (http-header-values headers "host"))
            (host (and (= (length host-values) 1) (first host-values)))
            (query-position (position #\? target))
            (path (if query-position
                      (subseq target 0 query-position)
                      target))
            (query (and query-position
                        (subseq target (1+ query-position)))))
       (unless host
         (error 'http-protocol-error
                :message "Origin-form requests require exactly one Host field."
                :operation :http1-parse
                :detail host-values))
       (make-http-uri :scheme scheme :authority host :path path :query query)))
    (:authority
     (make-http-uri :scheme scheme :authority target :path "/"))
    (:asterisk
     (let ((host-values (http-header-values headers "host")))
       (unless (= (length host-values) 1)
         (error 'http-protocol-error
                :message "Asterisk-form requests require exactly one Host field."
                :operation :http1-parse
                :detail host-values))
       (make-http-uri :scheme scheme :authority (first host-values) :path "/")))))

(defun %parse-http1-body (octets body-start framing max-body-bytes max-header-bytes
                           end-of-input)
  (case (%framing-kind framing)
    (:none (values (%empty-octets) nil body-start))
    (:content-length
     (let ((length (%framing-length framing)))
       (when (< (- (length octets) body-start) length)
         (%http1-incomplete (+ body-start length) (length octets)))
       (http-check-size-limit length max-body-bytes :body)
       (values (%octets-subseq octets body-start (+ body-start length))
               nil
               (+ body-start length))))
    (:chunked
     (%parse-chunked-at octets body-start max-body-bytes max-header-bytes))
    ((:close-delimited :tunnel)
     (unless end-of-input
       (%http1-incomplete :end-of-input (length octets)))
     (let ((length (- (length octets) body-start)))
       (http-check-size-limit length max-body-bytes :body)
       (values (%octets-subseq octets body-start)
               nil
               (length octets))))))

(defun http1-parse-request
    (octets &key (max-header-bytes *default-max-header-bytes*)
                  (max-body-bytes *default-max-body-bytes*)
                  (scheme "http"))
  "Parse one HTTP/1 request and return the request and unconsumed octets."
  (let ((copy (%copy-octets octets)))
    (multiple-value-bind (line headers body-start)
        (%parse-http1-header-block copy 0 max-header-bytes)
      (multiple-value-bind (method target version)
          (%parse-request-line line)
        (%validate-http1-host-header headers t version)
        (let* ((uri (%request-uri-from-target target headers scheme))
               (framing (%make-framing :request-p t
                                       :headers headers
                                       :request-method method)))
          (multiple-value-bind (body trailers consumed)
              (%parse-http1-body copy body-start framing max-body-bytes
                                 max-header-bytes t)
            (values
             (make-http-request :method method
                                :uri uri
                                :request-target target
                                :headers headers
                                :trailers trailers
                                :body body
                                :protocol-version version)
             (%octets-subseq copy consumed))))))))

(defun http1-parse-response
    (octets &key (max-header-bytes *default-max-header-bytes*)
                  (max-body-bytes *default-max-body-bytes*)
                  request-method
                  (end-of-input t))
  "Parse one HTTP/1 response and return the response and unconsumed octets."
  (let ((copy (%copy-octets octets)))
    (multiple-value-bind (line headers body-start)
        (%parse-http1-header-block copy 0 max-header-bytes)
      (multiple-value-bind (version status reason)
          (%parse-response-line line)
        (let ((framing (%make-framing :headers headers
                                      :status status
                                      :request-method request-method)))
          (multiple-value-bind (body trailers consumed)
              (%parse-http1-body copy body-start framing max-body-bytes
                                 max-header-bytes end-of-input)
             (values
             (make-http-response :status status
                                 :reason reason
                                 :headers headers
                                 :trailers trailers
                                 :body (if (http-response-body-allowed-p
                                            status request-method)
                                           body
                                           (%empty-octets))
                                 :protocol-version version)
             (%octets-subseq copy consumed))))))))

(defstruct (http1-parser
             (:constructor %make-http1-parser)
             (:conc-name %http1-parser-))
  request-p
  max-header-bytes
  max-body-bytes
  scheme
  request-method
  (buffer (%empty-octets))
  message
  remainder
  complete-p
  end-of-input-p
  incomplete-condition)

(defun make-http1-parser
    (&key (message-type :request)
          (max-header-bytes *default-max-header-bytes*)
          (max-body-bytes *default-max-body-bytes*)
          (scheme "http")
          request-method)
  "Create an incremental HTTP/1 request or response parser.

FEED appends octets until one message is complete.  A parser consumes one
message only; its returned remainder is the beginning of the next message."
  (unless (member message-type '(:request :response) :test #'eq)
    (error 'http-protocol-error
           :message "HTTP/1 parser message-type must be :REQUEST or :RESPONSE."
           :operation :http1-parser
           :detail message-type))
  (http-check-size-limit 0 max-header-bytes :headers)
  (http-check-size-limit 0 max-body-bytes :body)
  (unless (stringp scheme)
    (error 'http-protocol-error
           :message "HTTP/1 parser scheme must be a string."
           :operation :http1-parser
           :detail scheme))
  (%make-http1-parser
   :request-p (eq message-type :request)
   :max-header-bytes max-header-bytes
   :max-body-bytes max-body-bytes
   :scheme scheme
   :request-method request-method
   :remainder (%empty-octets)))

(defun http1-parser-message-type (parser)
  (if (%http1-parser-request-p parser) :request :response))

(defun http1-parser-message (parser)
  (%http1-parser-message parser))

(defun http1-parser-remainder (parser)
  (%copy-octets (or (%http1-parser-remainder parser) (%empty-octets))))

(defun http1-parser-buffered-bytes (parser)
  (length (%http1-parser-buffer parser)))

(defun http1-parser-complete-p (parser)
  (%http1-parser-complete-p parser))

(defun http1-parser-end-of-input-p (parser)
  (%http1-parser-end-of-input-p parser))

(defun %http1-parser-raise-after-end-of-input (parser octets end-of-input)
  (when (%http1-parser-end-of-input-p parser)
    (unless (and end-of-input (zerop (length octets)))
      (error 'http-protocol-error
             :message "HTTP/1 parser received octets after end-of-input."
             :operation :http1-parser
             :detail (length octets)))))

(defun %http1-parser-parse (parser)
  (handler-case
      (let ((buffer (%http1-parser-buffer parser)))
        (unless (search #(#x0d #x0a #x0d #x0a) buffer)
          (http-check-size-limit
           (length buffer)
           (%http1-parser-max-header-bytes parser)
           :headers))
      (multiple-value-bind (message remainder)
          (if (%http1-parser-request-p parser)
              (http1-parse-request
               (%http1-parser-buffer parser)
               :max-header-bytes (%http1-parser-max-header-bytes parser)
               :max-body-bytes (%http1-parser-max-body-bytes parser)
               :scheme (%http1-parser-scheme parser))
              (http1-parse-response
               (%http1-parser-buffer parser)
               :max-header-bytes (%http1-parser-max-header-bytes parser)
               :max-body-bytes (%http1-parser-max-body-bytes parser)
               :request-method (%http1-parser-request-method parser)
               :end-of-input (%http1-parser-end-of-input-p parser)))
        (setf (%http1-parser-message parser) message
              (%http1-parser-remainder parser) (%copy-octets remainder)
              (%http1-parser-complete-p parser) t
              (%http1-parser-incomplete-condition parser) nil)
        (values message (%copy-octets remainder) t)))
    (http-incomplete-message (condition)
      (setf (%http1-parser-incomplete-condition parser) condition)
      (values nil (%empty-octets) nil))))

(defun http1-parser-feed (parser octets &key end-of-input)
  "Feed OCTETS to an incremental HTTP/1 parser.

Return three values: the completed message or NIL, unconsumed octets, and a
boolean indicating completion.  Incomplete input is retained and reported by
the third value; syntax and limit errors are signaled immediately."
  (let ((input (%copy-octets octets)))
    (when (%http1-parser-complete-p parser)
      (if (zerop (length input))
          (return-from http1-parser-feed
            (values (%http1-parser-message parser)
                    (http1-parser-remainder parser)
                    t))
          (error 'http-protocol-error
                 :message "HTTP/1 parser cannot consume a second message."
                 :operation :http1-parser
                 :detail (length input))))
    (%http1-parser-raise-after-end-of-input parser input end-of-input)
    (setf (%http1-parser-buffer parser)
          (%append-octets (%http1-parser-buffer parser) input))
    (when end-of-input
      (setf (%http1-parser-end-of-input-p parser) t))
    (%http1-parser-parse parser)))

(defun http1-parser-finish (parser)
  "Mark an incremental parser at end-of-input and return its message.

Signal the retained HTTP-INCOMPLETE-MESSAGE condition when the message is
not complete after the final input boundary."
  (multiple-value-bind (message remainder complete-p)
      (http1-parser-feed parser (%empty-octets) :end-of-input t)
    (if complete-p
        (values message remainder t)
        (error (%http1-parser-incomplete-condition parser)))))

(defun http1-parser-reset (parser)
  "Reset PARSER while retaining its configured limits and message type."
  (setf (%http1-parser-buffer parser) (%empty-octets)
        (%http1-parser-message parser) nil
        (%http1-parser-remainder parser) (%empty-octets)
        (%http1-parser-complete-p parser) nil
        (%http1-parser-end-of-input-p parser) nil
        (%http1-parser-incomplete-condition parser) nil)
  parser)
