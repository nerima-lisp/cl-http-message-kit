(in-package #:http-message-kit)

(defparameter *http2-frame-types*
  '((:data . 0)
    (:headers . 1)
    (:priority . 2)
    (:rst-stream . 3)
    (:settings . 4)
    (:push-promise . 5)
    (:ping . 6)
    (:goaway . 7)
    (:window-update . 8)
    (:continuation . 9)))

(defparameter *http3-frame-types*
  '((:data . 0)
    (:headers . 1)
    (:cancel-push . 3)
    (:settings . 4)
    (:push-promise . 5)
    (:goaway . 7)
    (:max-push-id . 13)))

(defparameter *http2-setting-ids*
  '((:header-table-size . 1)
    (:enable-push . 2)
    (:max-concurrent-streams . 3)
    (:initial-window-size . 4)
    (:max-frame-size . 5)
    (:max-header-list-size . 6)))

(defparameter *http3-setting-ids*
  '((:qpack-max-table-capacity . 1)
    (:max-field-section-size . 6)
    (:qpack-blocked-streams . 7)
    (:enable-connect-protocol . 8)
    (:h3-datagram . #x33)))

(defparameter *http2-connection-preface*
  "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")

(defparameter *http2-connection-specific-fields*
  '("connection" "keep-alive" "proxy-connection" "transfer-encoding" "upgrade"))

(defstruct (http-frame (:constructor %make-http-frame)
                       (:conc-name %frame-))
  protocol
  type
  flags
  stream-id
  payload)

(defun %frame-error (protocol type stream-id detail)
  (error 'http-invalid-frame
         :message "The HTTP/2 or HTTP/3 frame is invalid."
         :operation :frame
         :protocol protocol
         :frame-type type
         :stream-id stream-id
         :detail detail))

(defun %incomplete-frame (needed available)
  (error 'http-incomplete-message
         :message "The HTTP frame is incomplete."
         :operation :frame
         :needed needed
         :available available))

(defun %normalize-frame-protocol (protocol)
  (cond ((or (eq protocol :http/2)
             (eq protocol :http2)
             (and (stringp protocol)
                  (string-equal protocol "HTTP/2")))
         :http/2)
        ((or (eq protocol :http/3)
             (eq protocol :http3)
             (and (stringp protocol)
                  (string-equal protocol "HTTP/3")))
         :http/3)
        (t
         (error 'http-protocol-error
                :message "The frame protocol must be HTTP/2 or HTTP/3."
                :operation :frame
                :detail protocol))))

(defun %frame-type-key (designator table protocol)
  (cond
    ((integerp designator)
     (unless (<= 0 designator (if (eq protocol :http/2) #xff #x3fffffffffffffff))
       (%frame-error protocol designator nil :type-range))
     (or (car (rassoc designator table)) designator))
    ((or (symbolp designator) (stringp designator))
     (let* ((name (if (symbolp designator)
                      (symbol-name designator)
                      designator))
            (entry (find-if (lambda (item)
                              (string-equal name (symbol-name (car item))))
                            table)))
           (if entry
               (car entry)
               (%frame-error protocol designator nil :unknown-type))))
    (t
     (%frame-error protocol designator nil :type))))

(defun http2-frame-type-code (type)
  (let ((key (%frame-type-key type *http2-frame-types* :http/2)))
    (if (integerp key)
        key
        (cdr (assoc key *http2-frame-types* :test #'eq)))))

(defun http2-frame-type-name (code)
  (let ((key (%frame-type-key code *http2-frame-types* :http/2)))
    key))

(defun http3-frame-type-code (type)
  (let ((key (%frame-type-key type *http3-frame-types* :http/3)))
    (if (integerp key)
        key
        (cdr (assoc key *http3-frame-types* :test #'eq)))))

(defun http3-frame-type-name (code)
  (%frame-type-key code *http3-frame-types* :http/3))

(defun %validate-frame-stream-id (protocol stream-id)
  (unless (or (and (eq protocol :http/3) (null stream-id))
              (and (integerp stream-id)
                   (<= 0 stream-id)
                   (<= stream-id #x7fffffff)))
    (%frame-error protocol nil stream-id :stream-id-range))
  stream-id)

(defun %validate-frame-flags (protocol flags)
  (unless (and (integerp flags) (<= 0 flags #xff))
    (%frame-error protocol nil nil :flags-range))
  (when (and (eq protocol :http/3) (plusp flags))
    (%frame-error protocol nil nil :flags-not-supported))
  flags)

(defun %http-frame (protocol type flags stream-id payload)
  (let* ((protocol (%normalize-frame-protocol protocol))
         (table (if (eq protocol :http/2)
                    *http2-frame-types*
                    *http3-frame-types*)))
    (%frame-type-key type table protocol)
    (%validate-frame-flags protocol flags)
    (%validate-frame-stream-id protocol stream-id)
    (%make-http-frame
     :protocol protocol
     :type (if (eq protocol :http/2)
               (%frame-type-key type *http2-frame-types* protocol)
               (%frame-type-key type *http3-frame-types* protocol))
     :flags flags
     :stream-id stream-id
     :payload (%copy-octets payload))))

(defun make-http-frame (&key (protocol :http/2) type (flags 0) stream-id payload)
  (unless type
    (error 'http-protocol-error
           :message "HTTP frames require a frame type."
           :operation :frame
           :detail type))
  (%http-frame protocol type flags
               (if (and (null stream-id)
                        (eq (%normalize-frame-protocol protocol) :http/2))
                   0
                   stream-id)
               payload))

(defun http-frame-protocol (frame)
  (check-type frame http-frame)
  (%normalize-frame-protocol (%frame-protocol frame)))

(defun http-frame-type (frame)
  (check-type frame http-frame)
  (%frame-type-key (%frame-type frame)
                   (if (eq (%frame-protocol frame) :http/2)
                       *http2-frame-types*
                       *http3-frame-types*)
                   (%frame-protocol frame)))

(defun http-frame-flags (frame)
  (check-type frame http-frame)
  (%frame-flags frame))

(defun http-frame-stream-id (frame)
  (check-type frame http-frame)
  (%frame-stream-id frame))

(defun http-frame-payload (frame)
  (check-type frame http-frame)
  (%copy-octets (%frame-payload frame)))

(defun %raw-header-name (header)
  (cond ((http-header-p header) (http-header-name header))
        ((and (consp header) (stringp (car header)))
         (car header))
        (t nil)))

(defun %header-section-error (protocol name detail)
  (error 'http-invalid-header
         :message "The HTTP/2 or HTTP/3 header section is invalid."
         :operation protocol
         :name name
         :reason detail))

(defun %header-section-value (headers name)
  (let ((values (http-header-values headers name)))
    (when (> (length values) 1)
      (%header-section-error :header-section name :duplicate))
    (first values)))

(defun %header-section-pseudo-values (headers)
  (loop for header in headers
        when (http-pseudo-header-p header)
          collect (cons (http-header-name header)
                        (http-header-content header))))

(defun %validate-header-section (headers protocol &key request-p trailers-p)
  (let ((normalized (%normalize-headers headers))
        (regular-seen nil)
        (pseudo-names nil))
    (loop for original in headers
          for header in normalized
          for raw-name = (%raw-header-name original)
          for name = (http-header-name header)
          do
             (unless (and raw-name (string= raw-name (%ascii-lowercase raw-name)))
               (%header-section-error protocol raw-name :uppercase-name))
             (if (http-pseudo-header-p header)
                 (progn
                   (when regular-seen
                     (%header-section-error protocol name :pseudo-after-regular))
                   (when (member name pseudo-names :test #'string=)
                     (%header-section-error protocol name :duplicate-pseudo))
                   (push name pseudo-names))
                 (progn
                   (setf regular-seen t)
                   (when (member name *http2-connection-specific-fields*
                                 :test #'string=)
                     (%header-section-error protocol name :connection-specific))
                   (when (and (string= name "te")
                              (not (string-equal
                                    (%trim-ows (http-header-content header))
                                    "trailers")))
                     (%header-section-error protocol name :te-value)))))
    (let* ((pseudo (%header-section-pseudo-values normalized))
           (method (cdr (assoc ":method" pseudo :test #'string=)))
           (scheme (cdr (assoc ":scheme" pseudo :test #'string=)))
           (path (cdr (assoc ":path" pseudo :test #'string=)))
           (authority (cdr (assoc ":authority" pseudo :test #'string=)))
           (protocol-name (cdr (assoc ":protocol" pseudo :test #'string=)))
           (status (cdr (assoc ":status" pseudo :test #'string=)))
           (host (%header-section-value normalized "host")))
      (dolist (item pseudo)
        (unless (member (car item)
                        '(":method" ":scheme" ":authority" ":path"
                          ":protocol" ":status")
                        :test #'string=)
          (%header-section-error protocol (car item) :unknown-pseudo-header)))
      (if trailers-p
          (when pseudo
            (%header-section-error protocol (caar pseudo) :pseudo-in-trailers))
          (if request-p
              (progn
                (when status
                  (%header-section-error protocol ":status" :response-pseudo-header))
                (unless method
                  (%header-section-error protocol ":method" :missing))
                (unless (%token-p method)
                  (%header-section-error protocol ":method" :value))
                (when scheme
                  (unless (%token-p scheme)
                    (%header-section-error protocol ":scheme" :value)))
                (when authority
                  (unless (plusp (length authority))
                    (%header-section-error protocol ":authority" :value)))
                (when path
                  (unless (or (string= path "*")
                              (and (plusp (length path))
                                   (char= (char path 0) #\/)
                                   (not (find #\# path))))
                    (%header-section-error protocol ":path" :value)))
                (if (http-method-connect-p method)
                    (if protocol-name
                        (unless (and scheme path authority
                                     (%token-p protocol-name))
                          (%header-section-error protocol ":protocol"
                                                  :extended-connect-fields))
                        (progn
                          (when scheme
                            (%header-section-error protocol ":scheme"
                                                   :classic-connect-field))
                          (when path
                            (%header-section-error protocol ":path"
                                                   :classic-connect-field))
                          (unless authority
                            (%header-section-error protocol ":authority" :missing))))
                    (progn
                      (when protocol-name
                        (%header-section-error protocol ":protocol"
                                                :non-connect-field))
                      (unless (and scheme path)
                        (%header-section-error protocol ":scheme-or-path" :missing))
                      (unless authority
                        (%header-section-error protocol ":authority" :missing))))
                (when (and authority host
                           (not (string-equal authority host)))
                  (%header-section-error protocol ":authority" :host-mismatch)))
              (progn
                (dolist (name '(":method" ":scheme" ":path" ":authority"
                               ":protocol") )
                  (when (assoc name pseudo :test #'string=)
                    (%header-section-error protocol name
                                            :request-pseudo-header)))
                (unless status
                  (%header-section-error protocol ":status" :missing))
                (unless (and (= (length status) 3)
                             (%decimal-string-p status)
                             (<= 100 (%parse-decimal status) 599))
                  (%header-section-error protocol ":status" :value)))))
      normalized)))

(defun http-validate-header-section (headers protocol &key request-p trailers-p)
  (%validate-header-section headers (%normalize-frame-protocol protocol)
                            :request-p request-p
                            :trailers-p trailers-p))

(defun http2-validate-header-section (headers &key request-p trailers-p)
  (%validate-header-section headers :http/2
                            :request-p request-p
                            :trailers-p trailers-p))

(defun http3-validate-header-section (headers &key request-p trailers-p)
  (%validate-header-section headers :http/3
                            :request-p request-p
                            :trailers-p trailers-p))

(defun http2-connection-preface ()
  (%octets-from-string *http2-connection-preface* :ascii-only t))

(defun http2-connection-preface-p (octets)
  (let ((value (%copy-octets octets)))
    (and (= (length value) (length *http2-connection-preface*))
         (equalp value (http2-connection-preface)))))

(defun %frame-octets-subseq (octets start &optional end)
  (subseq octets start (or end (length octets))))

(defun %u16-octets (value)
  (vector (ldb (byte 8 8) value)
          (ldb (byte 8 0) value)))

(defun %u24-octets (value)
  (vector (ldb (byte 8 16) value)
          (ldb (byte 8 8) value)
          (ldb (byte 8 0) value)))

(defun %u32-octets (value)
  (vector (ldb (byte 8 24) value)
          (ldb (byte 8 16) value)
          (ldb (byte 8 8) value)
          (ldb (byte 8 0) value)))

(defun %u16-from-octets (octets start)
  (+ (ash (aref octets start) 8)
     (aref octets (1+ start))))

(defun %u24-from-octets (octets start)
  (+ (ash (aref octets start) 16)
     (ash (aref octets (1+ start)) 8)
     (aref octets (+ start 2))))

(defun %u31-from-octets (octets start)
  (logand #x7fffffff
          (+ (ash (aref octets start) 24)
             (ash (aref octets (1+ start)) 16)
             (ash (aref octets (+ start 2)) 8)
             (aref octets (+ start 3)))))

(defun %u32-from-octets (octets start)
  (+ (ash (aref octets start) 24)
     (ash (aref octets (1+ start)) 16)
     (ash (aref octets (+ start 2)) 8)
     (aref octets (+ start 3))))

(defun %http2-payload-bounds (type flags stream-id payload minimum)
  (let* ((padded-p (logtest #x8 flags))
         (padding-length (if padded-p
                             (if (plusp (length payload))
                                 (aref payload 0)
                                 (%frame-error :http/2 type stream-id
                                               :padding-shape))
                             0))
         (start (if padded-p 1 0))
         (end (- (length payload) padding-length)))
    (when (< end (+ start minimum))
      (%frame-error :http/2 type stream-id :padding-shape))
    (values start end)))

(defun %validate-http2-flags (type flags stream-id)
  (let ((mask (case type
                (:data #x09)
                (:headers #x2d)
                (:settings #x01)
                (:push-promise #x0c)
                (:continuation #x04)
                (otherwise nil))))
    (when (and mask
               (plusp (logand flags (logxor #xff mask))))
      (%frame-error :http/2 type stream-id :flags))))

(defun %validate-http2-frame (type flags stream-id payload)
  (let ((length (length payload)))
    (%validate-http2-flags type flags stream-id)
    (case type
      (:data
       (unless (plusp stream-id) (%frame-error :http/2 type stream-id :stream-id))
       (%http2-payload-bounds type flags stream-id payload 0))
      (:headers
       (unless (plusp stream-id) (%frame-error :http/2 type stream-id :stream-id))
       (multiple-value-bind (start end)
           (%http2-payload-bounds type flags stream-id payload
                                  (if (logtest #x20 flags) 5 0))
         (declare (ignore end))
         (when (logtest #x20 flags)
           (when (= (%u31-from-octets payload start) stream-id)
             (%frame-error :http/2 type stream-id
                           :priority-self-dependency)))))
      (:priority
       (unless (and (plusp stream-id) (= length 5))
         (%frame-error :http/2 type stream-id :priority-shape))
       (when (= (%u31-from-octets payload 1) stream-id)
         (%frame-error :http/2 type stream-id :priority-self-dependency)))
      (:rst-stream
       (unless (and (plusp stream-id) (= length 4))
         (%frame-error :http/2 type stream-id :rst-stream-shape)))
      (:settings
       (unless (zerop stream-id) (%frame-error :http/2 type stream-id :stream-id))
       (when (logtest 1 flags)
         (unless (zerop length)
           (%frame-error :http/2 type stream-id :settings-ack-payload)))
       (unless (zerop (mod length 6))
         (%frame-error :http/2 type stream-id :settings-shape))
       (http2-decode-settings payload))
      (:push-promise
       (unless (and (plusp stream-id) (>= length 4))
         (%frame-error :http/2 type stream-id :push-promise-shape))
       (multiple-value-bind (start end)
           (%http2-payload-bounds type flags stream-id payload 4)
         (declare (ignore end))
         (let ((raw-promised-id (%u32-from-octets payload start)))
           (when (logtest #x80000000 raw-promised-id)
             (%frame-error :http/2 type stream-id
                           :reserved-promised-stream-bit))
           (when (zerop (logand raw-promised-id #x7fffffff))
             (%frame-error :http/2 type stream-id
                           :promised-stream-id)))))
      (:ping
       (unless (and (zerop stream-id) (= length 8))
         (%frame-error :http/2 type stream-id :ping-shape)))
      (:goaway
       (unless (and (zerop stream-id) (>= length 8))
         (%frame-error :http/2 type stream-id :goaway-shape))
       (when (logtest #x80000000 (%u32-from-octets payload 0))
         (%frame-error :http/2 type stream-id :reserved-last-stream-bit)))
      (:window-update
       (unless (= length 4)
         (%frame-error :http/2 type stream-id :window-update-shape))
       (let ((raw-increment (%u32-from-octets payload 0)))
         (when (logtest #x80000000 raw-increment)
           (%frame-error :http/2 type stream-id :reserved-window-bit))
         (when (zerop (logand raw-increment #x7fffffff))
           (%frame-error :http/2 type stream-id :window-size))))
      (:continuation
       (unless (plusp stream-id)
         (%frame-error :http/2 type stream-id :stream-id))))
    t))

(defun %frame-with-overrides (protocol frame type type-p flags flags-p
                                stream-id stream-id-p payload payload-p)
  (if (http-frame-p frame)
      (progn
        (unless (eq (%frame-protocol frame) protocol)
          (%frame-error protocol (%frame-type frame) (%frame-stream-id frame)
                        :protocol-mismatch))
        (%http-frame protocol
                     (if type-p type (%frame-type frame))
                     (if flags-p flags (%frame-flags frame))
                     (if stream-id-p stream-id (%frame-stream-id frame))
                     (if payload-p payload (%frame-payload frame))))
      (%http-frame protocol
                   (if type-p type frame)
                   (if flags-p flags 0)
                   (if stream-id-p
                       stream-id
                       (if (eq protocol :http/3) nil 0))
                   (if payload-p payload nil))))

(defun http2-encode-frame
    (frame &key (type nil type-p) (flags 0 flags-p)
             (stream-id 0 stream-id-p) (payload nil payload-p))
  (let* ((value (%frame-with-overrides :http/2 frame type type-p flags flags-p
                                       stream-id stream-id-p payload payload-p))
         (frame-type (%frame-type value))
         (code (http2-frame-type-code frame-type))
         (flags (%frame-flags value))
         (stream-id (%frame-stream-id value))
         (payload (%frame-payload value)))
    (when (> (length payload) #xffffff)
      (%frame-error :http/2 frame-type stream-id :payload-too-large))
    (%validate-http2-frame frame-type flags stream-id payload)
    (let ((result (make-array (+ 9 (length payload))
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (replace result (%u24-octets (length payload)) :start1 0)
      (setf (aref result 3) code
            (aref result 4) flags)
      (replace result (%u32-octets stream-id) :start1 5)
      (replace result payload :start1 9)
      result)))

(defun http2-decode-frame (octets)
  (let ((copy (%copy-octets octets)))
    (when (< (length copy) 9)
      (%incomplete-frame 9 (length copy)))
    (let* ((length (%u24-from-octets copy 0))
           (code (aref copy 3))
           (flags (aref copy 4))
           (raw-stream-id (%u32-from-octets copy 5))
           (stream-id (logand #x7fffffff raw-stream-id))
           (type (http2-frame-type-name code)))
      (when (logtest #x80000000 raw-stream-id)
        (%frame-error :http/2 type stream-id :reserved-stream-bit))
      (when (< (length copy) (+ 9 length))
        (%incomplete-frame (+ 9 length) (length copy)))
      (let ((payload (%frame-octets-subseq copy 9 (+ 9 length))))
        (%validate-http2-frame type flags stream-id payload)
        (values (%http-frame :http/2 type flags stream-id payload)
                (+ 9 length))))))

(defun %quic-varint-width (value)
  (cond ((<= value #x3f) 1)
        ((<= value #x3fff) 2)
        ((<= value #x3fffffff) 4)
        ((<= value #x3fffffffffffffff) 8)
        (t nil)))

(defun http3-encode-varint (value)
  (unless (and (integerp value) (>= value 0))
    (error 'http-protocol-error
           :message "QUIC variable-length integers must be non-negative integers."
           :operation :http3-varint
           :detail value))
  (let ((width (%quic-varint-width value)))
    (unless width
      (error 'http-protocol-error
             :message "QUIC variable-length integers exceed 62 bits."
             :operation :http3-varint
             :detail value))
    (let ((result (make-array width :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (loop for index downfrom (1- width) to 0
            for shift from 0 by 8
            do (setf (aref result index) (ldb (byte 8 shift) value)))
      (setf (aref result 0)
            (logior (aref result 0)
                    (ecase width
                      (1 0)
                      (2 #x40)
                      (4 #x80)
                      (8 #xc0))))
      result)))

(defun http3-decode-varint (octets)
  (let ((copy (%copy-octets octets :allow-list nil)))
    (when (zerop (length copy))
      (%incomplete-frame 1 0))
    (let* ((prefix (ldb (byte 2 6) (aref copy 0)))
           (width (ecase prefix (0 1) (1 2) (2 4) (3 8))))
      (when (< (length copy) width)
        (%incomplete-frame width (length copy)))
      (let ((value (logand #x3f (aref copy 0))))
        (loop for index from 1 below width
              do (setf value (+ (ash value 8) (aref copy index))))
        (values value width)))))

(defun %setting-designator (designator table protocol)
  (cond ((integerp designator)
         (unless (<= 0 designator
                     (if (eq protocol :http/2)
                         #xffff
                         #x3fffffffffffffff))
           (%frame-error protocol :settings nil :setting-id))
         designator)
        ((or (symbolp designator) (stringp designator))
         (let* ((name (if (symbolp designator)
                          (symbol-name designator)
                          designator))
                (entry (find-if (lambda (item)
                                  (string-equal name (symbol-name (car item))))
                                table)))
           (if entry
               (cdr entry)
               (%frame-error protocol :settings nil :unknown-setting))))
        (t (%frame-error protocol :settings nil :setting-id))))

(defun %setting-entry (entry protocol table)
  (unless (consp entry)
    (%frame-error protocol :settings nil :setting-entry))
  (let* ((tail (cdr entry))
         (value (cond ((atom tail) tail)
                      ((null (cddr entry)) (cadr entry))
                      (t (%frame-error protocol :settings nil
                                       :setting-entry))))
         (id (%setting-designator (car entry) table protocol)))
    (unless (and (integerp value)
                 (<= 0 value
                     (if (eq protocol :http/2)
                         #xffffffff
                         #x3fffffffffffffff)))
      (%frame-error protocol :settings nil (list :setting-value id value)))
    (values id value)))

(defun %validate-http2-setting (id value)
  (case id
    (2 (unless (<= value 1) (%frame-error :http/2 :settings nil :enable-push)))
    (4 (unless (<= value #x7fffffff)
         (%frame-error :http/2 :settings nil :initial-window-size)))
    (5 (unless (<= #x4000 value #xffffff)
         (%frame-error :http/2 :settings nil :max-frame-size))))
  t)

(defun http2-encode-settings (settings &key ack)
  (when (and ack settings)
    (%frame-error :http/2 :settings 0 :ack-with-payload))
  (if ack
      (%empty-octets)
      (let ((seen nil))
        (apply #'%append-octets
               (mapcar (lambda (entry)
                         (multiple-value-bind (id value)
                             (%setting-entry entry :http/2 *http2-setting-ids*)
                           (when (member id seen)
                             (%frame-error :http/2 :settings 0
                                           :duplicate-setting))
                           (push id seen)
                           (%validate-http2-setting id value)
                           (%append-octets (%u16-octets id)
                                           (%u32-octets value))))
                       settings)))))

(defun %http2-setting-name (id)
  (or (car (rassoc id *http2-setting-ids*)) id))

(defun http2-decode-settings (octets)
  (let ((copy (%copy-octets octets :allow-list nil)))
    (unless (zerop (mod (length copy) 6))
      (%frame-error :http/2 :settings 0 :settings-shape))
    (let ((seen nil)
          (result nil))
      (loop for start from 0 below (length copy) by 6
            for id = (%u16-from-octets copy start)
            for value = (%u32-from-octets copy (+ start 2))
            do (when (member id seen)
                 (%frame-error :http/2 :settings 0 :duplicate-setting))
               (%validate-http2-setting id value)
               (push (cons (%http2-setting-name id) value) result)
               (push id seen))
      (nreverse result))))

(defun %http3-setting-name (id)
  (or (car (rassoc id *http3-setting-ids*)) id))

(defun %validate-http3-setting (id value)
  (when (and (member id '(8 #x33)) (> value 1))
    (%frame-error :http/3 :settings nil
                  (if (= id 8) :enable-connect-protocol :h3-datagram)))
  t)

(defun http3-encode-settings (settings)
  (let ((seen nil))
    (apply #'%append-octets
           (mapcar (lambda (entry)
                     (multiple-value-bind (id value)
                         (%setting-entry entry :http/3 *http3-setting-ids*)
                       (when (member id seen)
                         (%frame-error :http/3 :settings nil
                                       :duplicate-setting))
                       (push id seen)
                       (%validate-http3-setting id value)
                       (%append-octets (http3-encode-varint id)
                                       (http3-encode-varint value))))
                   settings))))

(defun http3-decode-settings (octets)
  (let ((copy (%copy-octets octets :allow-list nil))
        (position 0)
        (seen nil)
        (result nil))
    (loop while (< position (length copy))
          do (multiple-value-bind (id used-id)
               (http3-decode-varint (%frame-octets-subseq copy position))
               (incf position used-id)
               (multiple-value-bind (value used-value)
                   (http3-decode-varint (%frame-octets-subseq copy position))
                 (incf position used-value)
                 (when (member id seen)
                   (%frame-error :http/3 :settings nil :duplicate-setting))
                 (%validate-http3-setting id value)
                 (push (cons (%http3-setting-name id) value) result)
                 (push id seen))))
    (nreverse result)))

(defun %validate-http3-frame (type payload)
  (case type
    ((:cancel-push :goaway :max-push-id)
     (multiple-value-bind (value used)
         (http3-decode-varint payload)
       (declare (ignore value))
       (unless (= used (length payload))
         (%frame-error :http/3 type nil :payload-shape))))
    (:settings (http3-decode-settings payload))
    (:push-promise
     (multiple-value-bind (value used)
         (http3-decode-varint payload)
       (declare (ignore value))
       (unless (= used (length payload))
         (%frame-error :http/3 type nil :payload-shape)))))
  t)

(defun http3-encode-frame
    (frame &key (type nil type-p) (flags 0 flags-p)
             (stream-id nil stream-id-p) (payload nil payload-p))
  (let* ((value (%frame-with-overrides :http/3 frame type type-p flags flags-p
                                       stream-id stream-id-p payload payload-p))
         (frame-type (%frame-type value))
         (code (http3-frame-type-code frame-type))
         (payload (%frame-payload value)))
    (%validate-http3-frame frame-type payload)
    (%append-octets (http3-encode-varint code)
                    (http3-encode-varint (length payload))
                    payload)))

(defun http3-decode-frame (octets &key stream-id)
  (let ((copy (%copy-octets octets :allow-list nil))
        (position 0))
    (multiple-value-bind (code type-bytes)
        (http3-decode-varint copy)
      (incf position type-bytes)
      (multiple-value-bind (length length-bytes)
                 (http3-decode-varint (%frame-octets-subseq copy position))
        (incf position length-bytes)
        (when (< (length copy) (+ position length))
          (%incomplete-frame (+ position length) (length copy)))
        (let* ((type (http3-frame-type-name code))
               (payload (%frame-octets-subseq copy position (+ position length))))
          (%validate-frame-stream-id :http/3 stream-id)
          (%validate-http3-frame type payload)
          (values (%http-frame :http/3 type 0 stream-id payload)
                  (+ position length)))))))
