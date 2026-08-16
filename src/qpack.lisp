(in-package #:http-message-kit)

(defparameter *qpack-static-table*
  #((":authority" "")
    (":path" "/")
    ("age" "0")
    ("content-disposition" "")
    ("content-length" "0")
    ("cookie" "")
    ("date" "")
    ("etag" "")
    ("if-modified-since" "")
    ("if-none-match" "")
    ("last-modified" "")
    ("link" "")
    ("location" "")
    ("referer" "")
    ("set-cookie" "")
    (":method" "CONNECT")
    (":method" "DELETE")
    (":method" "GET")
    (":method" "HEAD")
    (":method" "OPTIONS")
    (":method" "POST")
    (":method" "PUT")
    (":scheme" "http")
    (":scheme" "https")
    (":status" "103")
    (":status" "200")
    (":status" "304")
    (":status" "404")
    (":status" "503")
    ("accept" "*/*")
    ("accept" "application/dns-message")
    ("accept-encoding" "gzip, deflate, br")
    ("accept-ranges" "bytes")
    ("access-control-allow-headers" "cache-control")
    ("access-control-allow-headers" "content-type")
    ("access-control-allow-origin" "*")
    ("cache-control" "max-age=0")
    ("cache-control" "max-age=2592000")
    ("cache-control" "max-age=604800")
    ("cache-control" "no-cache")
    ("cache-control" "no-store")
    ("cache-control" "public, max-age=31536000")
    ("content-encoding" "br")
    ("content-encoding" "gzip")
    ("content-type" "application/dns-message")
    ("content-type" "application/javascript")
    ("content-type" "application/json")
    ("content-type" "application/x-www-form-urlencoded")
    ("content-type" "image/gif")
    ("content-type" "image/jpeg")
    ("content-type" "image/png")
    ("content-type" "text/css")
    ("content-type" "text/html; charset=utf-8")
    ("content-type" "text/plain")
    ("content-type" "text/plain;charset=utf-8")
    ("range" "bytes=0-")
    ("strict-transport-security" "max-age=31536000")
    ("strict-transport-security" "max-age=31536000; includesubdomains")
    ("strict-transport-security"
     "max-age=31536000; includesubdomains; preload")
    ("vary" "accept-encoding")
    ("vary" "origin")
    ("x-content-type-options" "nosniff")
    ("x-xss-protection" "1; mode=block")
    (":status" "100")
    (":status" "204")
    (":status" "206")
    (":status" "302")
    (":status" "400")
    (":status" "403")
    (":status" "421")
    (":status" "425")
    (":status" "500")
    ("accept-language" "")
    ("access-control-allow-credentials" "FALSE")
    ("access-control-allow-credentials" "TRUE")
    ("access-control-allow-headers" "*")
    ("access-control-allow-methods" "get")
    ("access-control-allow-methods" "get, post, options")
    ("access-control-allow-methods" "options")
    ("access-control-expose-headers" "content-length")
    ("access-control-request-headers" "content-type")
    ("access-control-request-method" "get")
    ("access-control-request-method" "post")
    ("alt-svc" "clear")
    ("authorization" "")
    ("content-security-policy"
     "script-src 'none'; object-src 'none'; base-uri 'none'")
    ("early-data" "1")
    ("expect-ct" "")
    ("forwarded" "")
    ("if-range" "")
    ("origin" "")
    ("purpose" "prefetch")
    ("server" "")
    ("timing-allow-origin" "*")
    ("upgrade-insecure-requests" "1")
    ("user-agent" "")
    ("x-forwarded-for" "")
    ("x-frame-options" "deny")
    ("x-frame-options" "sameorigin")))

(defstruct (qpack-entry
            (:constructor %make-qpack-entry (name value size absolute-index)))
  name
  value
  size
  absolute-index)

(defstruct (qpack-context
            (:constructor %make-qpack-context
                (max-table-capacity table-capacity max-blocked-streams
                 max-header-list-size huffman-p dynamic-table dynamic-size
                 insert-count known-received-count))
            (:conc-name qpack-context-))
  max-table-capacity
  table-capacity
  max-blocked-streams
  max-header-list-size
  huffman-p
  dynamic-table
  dynamic-size
  insert-count
  known-received-count)

(defun %qpack-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :qpack
         :detail detail))

(defun %qpack-incomplete (needed available)
  (error 'http-incomplete-message
         :message "QPACK input is incomplete."
         :operation :qpack
         :needed needed
         :available available))

(defun %qpack-octets (value)
  (handler-case
      (%copy-octets value :allow-list t)
    (http-protocol-error ()
      (%qpack-error "QPACK input must be a one-dimensional octet sequence."
                    (type-of value)))))

(defun %qpack-validate-context (context)
  (unless (qpack-context-p context)
    (%qpack-error "QPACK context is invalid." (type-of context)))
  context)

(defun %qpack-nonnegative-integer (value name)
  (unless (and (integerp value) (>= value 0))
    (%qpack-error (format nil "~A must be a non-negative integer." name)
                  value))
  value)

(defun %qpack-boolean (value name)
  (unless (member value '(nil t))
    (%qpack-error (format nil "~A must be boolean." name) value))
  value)

(defun make-qpack-context (&key (max-table-capacity 4096)
                                (table-capacity max-table-capacity)
                                (max-blocked-streams 0)
                                (max-header-list-size *default-max-header-bytes*)
                                (huffman t))
  (%qpack-nonnegative-integer max-table-capacity :max-table-capacity)
  (%qpack-nonnegative-integer table-capacity :table-capacity)
  (when (> table-capacity max-table-capacity)
    (%qpack-error "QPACK table capacity exceeds the configured maximum."
                  table-capacity))
  (%qpack-nonnegative-integer max-blocked-streams :max-blocked-streams)
  (unless (or (null max-header-list-size)
              (and (integerp max-header-list-size)
                   (>= max-header-list-size 0)))
    (%qpack-error "QPACK maximum header list size must be a non-negative integer or NIL."
                  max-header-list-size))
  (%qpack-boolean huffman :huffman)
  (%make-qpack-context max-table-capacity table-capacity max-blocked-streams
                       max-header-list-size huffman nil 0 0 0))

(defun qpack-context-table-size (context)
  (%qpack-validate-context context)
  (qpack-context-dynamic-size context))

(defun qpack-context-dynamic-entry-count (context)
  (%qpack-validate-context context)
  (length (qpack-context-dynamic-table context)))

(defun %qpack-static-entry (index)
  (when (and (integerp index)
             (<= 0 index)
             (< index (length *qpack-static-table*)))
    (let ((entry (aref *qpack-static-table* index)))
      (values (first entry) (second entry)))))

(defun %qpack-entry-size (name value)
  (+ 32 (length (%octets-from-string name))
     (length (%octets-from-string value))))

(defun %qpack-evict-to (context limit)
  (loop for oldest = (car (last (qpack-context-dynamic-table context)))
        while (> (qpack-context-dynamic-size context) limit)
        do (unless oldest
             (%qpack-error "QPACK dynamic table accounting is inconsistent."))
           (setf (qpack-context-dynamic-table context)
                 (butlast (qpack-context-dynamic-table context)))
           (decf (qpack-context-dynamic-size context)
                 (qpack-entry-size oldest)))
  context)

(defun qpack-context-set-capacity (context capacity)
  (%qpack-validate-context context)
  (%qpack-nonnegative-integer capacity :capacity)
  (when (> capacity (qpack-context-max-table-capacity context))
    (%qpack-error "QPACK table capacity exceeds the configured maximum."
                  capacity))
  (setf (qpack-context-table-capacity context) capacity)
  (%qpack-evict-to context capacity)
  context)

(defun %qpack-add-entry (context name value)
  (let* ((size (%qpack-entry-size name value))
         (capacity (qpack-context-table-capacity context))
         (absolute-index (qpack-context-insert-count context)))
    (incf (qpack-context-insert-count context))
    (if (> size capacity)
        (%qpack-evict-to context 0)
        (progn
          (%qpack-evict-to context (- capacity size))
          (push (%make-qpack-entry name value size absolute-index)
                (qpack-context-dynamic-table context))
          (incf (qpack-context-dynamic-size context) size))))
  context)

(defun %qpack-absolute-entry (context absolute-index)
  (%qpack-nonnegative-integer absolute-index :absolute-index)
  (unless (< absolute-index (qpack-context-insert-count context))
    (%qpack-error "QPACK dynamic table entry is not available."
                  absolute-index))
  (or (find absolute-index (qpack-context-dynamic-table context)
            :key #'qpack-entry-absolute-index)
      (%qpack-error "QPACK dynamic table entry has been evicted."
                    absolute-index)))

(defun %qpack-relative-entry (context base relative-index)
  (%qpack-nonnegative-integer base :base)
  (%qpack-nonnegative-integer relative-index :relative-index)
  (when (>= relative-index base)
    (%qpack-error "QPACK relative index is outside the base."
                  relative-index))
  (%qpack-absolute-entry context (- base relative-index 1)))

(defun %qpack-post-base-entry (context base post-base-index)
  (%qpack-nonnegative-integer base :base)
  (%qpack-nonnegative-integer post-base-index :post-base-index)
  (%qpack-absolute-entry context (+ base post-base-index)))

(defun %qpack-find-exact-static (name value)
  (loop for index from 0 below (length *qpack-static-table*)
        for entry = (aref *qpack-static-table* index)
        when (and (string= name (first entry))
                  (string= value (second entry)))
          do (return index)))

(defun %qpack-find-name-static (name)
  (loop for index from 0 below (length *qpack-static-table*)
        for entry = (aref *qpack-static-table* index)
        when (string= name (first entry))
          do (return index)))

(defun %qpack-find-exact-dynamic (context name value)
  (find-if (lambda (entry)
             (and (string= name (qpack-entry-name entry))
                  (string= value (qpack-entry-value entry))))
           (qpack-context-dynamic-table context)))

(defun %qpack-find-name-dynamic (context name)
  (find-if (lambda (entry)
             (string= name (qpack-entry-name entry)))
           (qpack-context-dynamic-table context)))

(defun %qpack-encode-prefixed-string (string length-prefix-bits prefix-value
                                       huffman)
  (unless (stringp string)
    (%qpack-error "QPACK string must be a string." (type-of string)))
  (%qpack-boolean huffman :huffman)
  (let* ((plain (%octets-from-string string))
         (payload (if huffman
                      (%hpack-huffman-encode plain)
                      plain))
         (first (hpack-encode-integer
                 (length payload)
                 length-prefix-bits
                 (logior prefix-value
                         (if huffman
                             (ash 1 length-prefix-bits)
                             0)))))
    (%append-octets first payload)))

(defun %qpack-decode-integer (bytes start prefix-bits &optional end)
  (let ((limit (if (null end) (length bytes) end)))
    (unless (and (integerp start) (<= 0 start))
    (%qpack-error "QPACK integer start is invalid." start))
    (unless (and (integerp limit) (<= start limit (length bytes)))
      (%qpack-error "QPACK integer end is invalid." limit))
    (unless (and (integerp prefix-bits) (<= 1 prefix-bits 8))
      (%qpack-error "QPACK integer prefix width must be between one and eight."
                    prefix-bits))
    (when (>= start limit)
      (%qpack-incomplete (1+ start) limit))
    (let* ((mask (1- (ash 1 prefix-bits)))
           (first (aref bytes start))
           (initial (logand first mask)))
      (if (< initial mask)
          (values initial (1+ start))
          (let ((value mask)
                (multiplier 1)
                (position (1+ start)))
            (loop
              (when (>= position limit)
                (%qpack-incomplete (1+ position) limit))
              (let ((byte (aref bytes position)))
                (incf value (* (logand byte #x7f) multiplier))
                (if (< byte 128)
                    (return (values value (1+ position)))
                    (progn
                      (setf multiplier (* multiplier 128))
                      (when (> multiplier (ash 1 62))
                        (%qpack-error "QPACK integer is too large.")))))
              (incf position)))))))

(defun %qpack-decode-prefixed-string (bytes start length-prefix-bits &optional end)
  (let ((limit (if (null end) (length bytes) end)))
    (unless (and (integerp start) (<= 0 start))
      (%qpack-error "QPACK string start is invalid." start))
    (unless (and (integerp limit) (<= start limit (length bytes)))
      (%qpack-error "QPACK string end is invalid." limit))
    (when (>= start limit)
      (%qpack-incomplete (1+ start) limit))
    (let ((huffman (logbitp length-prefix-bits (aref bytes start))))
      (multiple-value-bind (length position)
          (%qpack-decode-integer bytes start length-prefix-bits limit)
        (let ((payload-end (+ position length)))
          (when (> payload-end limit)
            (%qpack-incomplete payload-end limit))
          (let ((payload (subseq bytes position payload-end)))
            (values (if huffman
                        (%string-from-octets (%hpack-huffman-decode payload))
                        (%string-from-octets payload))
                    payload-end)))))))

(defun %qpack-decode-standard-string (bytes start &optional end)
  (%qpack-decode-prefixed-string bytes start 7 end))

(defun %qpack-encode-standard-string (string huffman)
  (hpack-encode-string string :huffman huffman))

(defun %qpack-required-insert-count-encode (context required)
  (%qpack-nonnegative-integer required :required-insert-count)
  (if (zerop required)
      (vector 0)
      (let* ((max-entries (floor (qpack-context-max-table-capacity context) 32))
             (full-range (* 2 max-entries)))
        (when (zerop max-entries)
          (%qpack-error "QPACK cannot encode a dynamic reference with zero capacity."))
        (hpack-encode-integer (1+ (mod required full-range)) 8))))

(defun %qpack-required-insert-count-decode (context encoded)
  (%qpack-nonnegative-integer encoded :encoded-required-insert-count)
  (if (zerop encoded)
      0
      (let* ((max-entries (floor (qpack-context-max-table-capacity context) 32))
             (full-range (* 2 max-entries))
             (known (qpack-context-insert-count context)))
        (when (zerop max-entries)
          (%qpack-error "QPACK received a dynamic reference with zero capacity."))
        (when (> encoded full-range)
          (%qpack-error "QPACK encoded Required Insert Count is outside its range."
                        encoded))
        (let* ((max-value (+ known max-entries))
               (max-wrapped (* (floor max-value full-range) full-range))
               (candidate (+ max-wrapped (1- encoded))))
          (when (> candidate max-value)
            (if (<= candidate full-range)
                (%qpack-error
                 "QPACK Required Insert Count cannot be reconstructed."
                 encoded)
                (decf candidate full-range)))
          (when (zerop candidate)
            (%qpack-error "QPACK Required Insert Count cannot be zero here."
                          encoded))
          candidate))))

(defun %qpack-header-list (headers)
  (unless (%proper-list-p headers)
    (%qpack-error "QPACK headers must be a proper list."))
  (%normalize-headers headers))

(defun %qpack-header (name value)
  (%hpack-validate-header-name name)
  (make-http-header name value :pseudo-p (%pseudo-header-name-p name)))

(defun %qpack-reference-entry (context static-p index base)
  (if static-p
      (multiple-value-bind (name value) (%qpack-static-entry index)
        (unless name
          (%qpack-error "QPACK static table index is outside the table." index))
        (%qpack-header name value))
      (let ((entry (%qpack-relative-entry context base index)))
        (%qpack-header (qpack-entry-name entry)
                       (qpack-entry-value entry)))))

(defun %qpack-post-reference-entry (context base index)
  (let ((entry (%qpack-post-base-entry context base index)))
    (%qpack-header (qpack-entry-name entry)
                   (qpack-entry-value entry))))

(defun %qpack-field-header-size (header)
  (%hpack-header-size header))

(defun qpack-encode-set-capacity (context capacity)
  (%qpack-validate-context context)
  (%qpack-nonnegative-integer capacity :capacity)
  (qpack-context-set-capacity context capacity)
  (hpack-encode-integer capacity 5 #x20))

(defun qpack-encode-insert-with-name-reference
    (context index value &key (table :static) (huffman nil huffman-supplied-p))
  (%qpack-validate-context context)
  (%qpack-nonnegative-integer index :index)
  (unless (member table '(:static :dynamic))
    (%qpack-error "QPACK insertion table must be :STATIC or :DYNAMIC." table))
  (unless (stringp value)
    (%qpack-error "QPACK insertion value must be a string." (type-of value)))
  (unless huffman-supplied-p
    (setf huffman (qpack-context-huffman-p context)))
  (%qpack-boolean huffman :huffman)
  (let ((name (if (eq table :static)
                  (multiple-value-bind (static-name static-value)
                      (%qpack-static-entry index)
                    (declare (ignore static-value))
                    (or static-name
                        (%qpack-error "QPACK static table index is invalid."
                                      index)))
                  (let ((entry (nth index
                                    (qpack-context-dynamic-table context))))
                    (or (and entry (qpack-entry-name entry))
                        (%qpack-error "QPACK dynamic name index is invalid."
                                      index))))))
    (let ((prefix (if (eq table :static) #xC0 #x80)))
      (prog1
          (%append-octets
           (hpack-encode-integer index 6 prefix)
           (%qpack-encode-standard-string value huffman))
        (%qpack-add-entry context name value)))))

(defun qpack-encode-insert-with-literal-name
    (context name value &key (huffman nil huffman-supplied-p))
  (%qpack-validate-context context)
  (%hpack-validate-header-name name)
  (unless (stringp value)
    (%qpack-error "QPACK insertion value must be a string." (type-of value)))
  (unless huffman-supplied-p
    (setf huffman (qpack-context-huffman-p context)))
  (%qpack-boolean huffman :huffman)
  (prog1
      (%append-octets
       (%qpack-encode-prefixed-string name 5 #x40 huffman)
       (%qpack-encode-standard-string value huffman))
    (%qpack-add-entry context name value)))

(defun qpack-encode-duplicate (context relative-index)
  (%qpack-validate-context context)
  (let ((entry (%qpack-relative-entry
                context (qpack-context-insert-count context) relative-index)))
    (prog1
        (hpack-encode-integer relative-index 5 0)
      (%qpack-add-entry context (qpack-entry-name entry)
                        (qpack-entry-value entry)))))

(defun %qpack-decode-encoder-instruction (context bytes position limit)
  (let ((first (aref bytes position)))
    (cond
      ((logbitp 7 first)
       (multiple-value-bind (index next)
           (%qpack-decode-integer bytes position 6 limit)
         (let ((static-p (logbitp 6 first)))
           (multiple-value-bind (value end)
               (%qpack-decode-standard-string bytes next limit)
             (let ((name
                     (if static-p
                         (multiple-value-bind (static-name static-value)
                             (%qpack-static-entry index)
                           (declare (ignore static-value))
                           (or static-name
                               (%qpack-error
                                "QPACK static name index is invalid."
                                index)))
                         (let ((entry
                                 (nth index
                                      (qpack-context-dynamic-table context))))
                           (or (and entry (qpack-entry-name entry))
                               (%qpack-error
                                "QPACK dynamic name index is invalid."
                                index))))))
               (%qpack-add-entry context name value)
               (values (list :insert name value
                             (1- (qpack-context-insert-count context)))
                       end))))))
      ((= (logand first #xC0) #x40)
       (let ((huffman (logbitp 5 first)))
         (multiple-value-bind (name next)
             (%qpack-decode-prefixed-string bytes position 5 limit)
           (%hpack-validate-header-name name)
           (multiple-value-bind (value end)
               (%qpack-decode-standard-string bytes next limit)
             (%qpack-add-entry context name value)
             (values (list :insert name value
                           (1- (qpack-context-insert-count context))
                           :huffman huffman)
                     end)))))
      ((= (logand first #xE0) #x20)
       (multiple-value-bind (capacity next)
           (%qpack-decode-integer bytes position 5 limit)
         (qpack-context-set-capacity context capacity)
         (values (list :set-capacity capacity) next)))
      (t
       (multiple-value-bind (relative-index next)
           (%qpack-decode-integer bytes position 5 limit)
         (let ((entry (%qpack-relative-entry
                       context (qpack-context-insert-count context)
                       relative-index)))
           (%qpack-add-entry context (qpack-entry-name entry)
                             (qpack-entry-value entry))
           (values (list :duplicate relative-index
                         (1- (qpack-context-insert-count context)))
                   next)))))))

(defun qpack-decode-encoder-stream (context octets &key (start 0) end)
  (%qpack-validate-context context)
  (let* ((bytes (%qpack-octets octets))
         (limit (if (null end) (length bytes) end))
         (position start)
         (events nil))
    (unless (and (integerp start) (<= 0 start limit))
      (%qpack-error "QPACK encoder stream start is invalid." start))
    (unless (and (integerp limit) (<= start limit (length bytes)))
      (%qpack-error "QPACK encoder stream end is invalid." limit))
    (loop while (< position limit)
          do (multiple-value-bind (event next)
                 (%qpack-decode-encoder-instruction
                  context bytes position limit)
               (push event events)
               (setf position next)))
    (values (nreverse events) position)))

(defun qpack-encode-section-ack (stream-id)
  (%qpack-nonnegative-integer stream-id :stream-id)
  (hpack-encode-integer stream-id 7 #x80))

(defun qpack-encode-stream-cancellation (stream-id)
  (%qpack-nonnegative-integer stream-id :stream-id)
  (hpack-encode-integer stream-id 6 #x40))

(defun qpack-encode-insert-count-increment (increment)
  (%qpack-nonnegative-integer increment :increment)
  (when (zerop increment)
    (%qpack-error "QPACK Insert Count Increment must be positive."))
  (hpack-encode-integer increment 6 0))

(defun qpack-decode-decoder-stream (context octets &key (start 0) end)
  (%qpack-validate-context context)
  (let* ((bytes (%qpack-octets octets))
         (limit (if (null end) (length bytes) end))
         (position start)
         (events nil))
    (unless (and (integerp start) (<= 0 start limit))
      (%qpack-error "QPACK decoder stream start is invalid." start))
    (unless (and (integerp limit) (<= start limit (length bytes)))
      (%qpack-error "QPACK decoder stream end is invalid." limit))
    (loop while (< position limit)
          do (let ((first (aref bytes position)))
               (cond
                 ((logbitp 7 first)
                  (multiple-value-bind (stream-id next)
                      (%qpack-decode-integer bytes position 7 limit)
                    (push (list :section-ack stream-id) events)
                    (setf position next)))
                 ((logbitp 6 first)
                  (multiple-value-bind (stream-id next)
                      (%qpack-decode-integer bytes position 6 limit)
                    (push (list :stream-cancellation stream-id) events)
                    (setf position next)))
                 (t
                  (multiple-value-bind (increment next)
                      (%qpack-decode-integer bytes position 6 limit)
                    (when (zerop increment)
                      (%qpack-error "QPACK Insert Count Increment is zero."))
                    (let ((known (+ (qpack-context-known-received-count context)
                                    increment)))
                      (when (> known (qpack-context-insert-count context))
                        (%qpack-error
                         "QPACK Insert Count Increment exceeds the insert count."
                         increment))
                      (setf (qpack-context-known-received-count context) known))
                    (push (list :insert-count-increment increment) events)
                    (setf position next))))))
    (values (nreverse events) position)))

(defun %qpack-never-indexed-p (header option)
  (if (functionp option)
      (not (null (funcall option header)))
      (or option (%sensitive-header-p (%header-name header)))))

(defun %qpack-add-required-reference (required entry)
  (max required (1+ (qpack-entry-absolute-index entry))))

(defun %qpack-encode-indexed-field (index static-p)
  (hpack-encode-integer index 6 (if static-p #xC0 #x80)))

(defun %qpack-encode-literal-name-reference
    (index static-p value never-indexed-p huffman)
  (%append-octets
   (hpack-encode-integer
    index 4
    (logior #x40
            (if never-indexed-p #x20 0)
            (if static-p #x10 0)))
   (%qpack-encode-standard-string value huffman)))

(defun %qpack-encode-literal-name (name value never-indexed-p huffman)
  (%append-octets
   (%qpack-encode-prefixed-string
    name 3 (logior #x20 (if never-indexed-p #x10 0)) huffman)
   (%qpack-encode-standard-string value huffman)))

(defun qpack-encode-header-block
    (headers &key context stream-id use-dynamic-p never-indexed-p
             (huffman nil huffman-supplied-p))
  (let ((context (or context (make-qpack-context)))
        (fields (%qpack-header-list headers))
        (parts nil)
        (required 0)
        (base 0))
    (%qpack-validate-context context)
    (when stream-id
      (%qpack-nonnegative-integer stream-id :stream-id))
    (%qpack-boolean use-dynamic-p :use-dynamic-p)
    (unless huffman-supplied-p
      (setf huffman (qpack-context-huffman-p context)))
    (%qpack-boolean huffman :huffman)
    (setf base (if use-dynamic-p
                   (qpack-context-insert-count context)
                   0))
    (dolist (header fields)
      (%hpack-validate-header-name (%header-name header))
      (let* ((name (%header-name header))
             (value (%header-content header))
             (never (%qpack-never-indexed-p header never-indexed-p))
             (static-exact (%qpack-find-exact-static name value))
             (dynamic-exact (and use-dynamic-p
                                 (%qpack-find-exact-dynamic context name value)))
             (static-name (%qpack-find-name-static name))
             (dynamic-name (and use-dynamic-p
                                (%qpack-find-name-dynamic context name))))
        (cond
          ((and (not never) static-exact)
           (push (%qpack-encode-indexed-field static-exact t) parts))
          ((and (not never) dynamic-exact)
           (let ((relative (- base (qpack-entry-absolute-index dynamic-exact) 1)))
             (when (< relative 0)
               (%qpack-error "QPACK dynamic reference is outside the base."))
             (setf required
                   (%qpack-add-required-reference required dynamic-exact))
             (push (%qpack-encode-indexed-field relative nil) parts)))
          (static-name
           (push (%qpack-encode-literal-name-reference
                  static-name t value never huffman)
                 parts))
          (dynamic-name
           (let ((relative (- base (qpack-entry-absolute-index dynamic-name) 1)))
             (when (< relative 0)
               (%qpack-error "QPACK dynamic name reference is outside the base."))
             (setf required
                   (%qpack-add-required-reference required dynamic-name))
             (push (%qpack-encode-literal-name-reference
                    relative nil value never huffman)
                   parts)))
          (t
           (push (%qpack-encode-literal-name name value never huffman) parts)))))
    (when (plusp required)
      (setf base (qpack-context-insert-count context)))
    (let ((encoded-base
            (if (>= base required)
                (hpack-encode-integer (- base required) 7 0)
                (hpack-encode-integer (- required base 1) 7 #x80))))
      (%append-octets
       (%qpack-required-insert-count-encode context required)
       encoded-base
       (apply #'%append-octets (nreverse parts))))))

(defun %qpack-decode-field (context bytes position base limit)
  (let ((first (aref bytes position)))
    (cond
      ((logbitp 7 first)
       (multiple-value-bind (index next)
           (%qpack-decode-integer bytes position 6 limit)
         (values (%qpack-reference-entry context (logbitp 6 first)
                                          index base)
                 next)))
      ((= (logand first #xF0) #x10)
       (multiple-value-bind (index next)
           (%qpack-decode-integer bytes position 4 limit)
         (values (%qpack-post-reference-entry context base index) next)))
      ((= (logand first #xC0) #x40)
       (let ((static-p (logbitp 4 first)))
         (multiple-value-bind (index next)
             (%qpack-decode-integer bytes position 4 limit)
           (multiple-value-bind (value end)
               (%qpack-decode-standard-string bytes next limit)
             (let ((header
                     (if static-p
                         (multiple-value-bind (name static-value)
                             (%qpack-static-entry index)
                           (declare (ignore static-value))
                           (unless name
                             (%qpack-error
                              "QPACK static name index is outside the table."
                              index))
                           (%qpack-header name value))
                         (let ((entry (%qpack-relative-entry context base index)))
                           (%qpack-header (qpack-entry-name entry) value)))))
               (values header end))))))
      ((= (logand first #xE0) #x20)
       (multiple-value-bind (name next)
           (%qpack-decode-prefixed-string bytes position 3 limit)
         (%hpack-validate-header-name name)
         (multiple-value-bind (value end)
             (%qpack-decode-standard-string bytes next limit)
           (values (%qpack-header name value) end))))
      ((= (logand first #xF0) 0)
       (multiple-value-bind (index next)
           (%qpack-decode-integer bytes position 3 limit)
         (multiple-value-bind (value end)
             (%qpack-decode-standard-string bytes next limit)
           (let ((entry (%qpack-post-base-entry context base index)))
             (values (%qpack-header (qpack-entry-name entry) value)
                     end)))))
      (t
       (%qpack-error "QPACK field line representation is invalid." first)))))

(defun qpack-decode-header-block
    (octets &key context stream-id (start 0) end)
  (let* ((context (or context (make-qpack-context)))
         (bytes (%qpack-octets octets))
         (limit (if (null end) (length bytes) end))
         (position start)
         (fields nil)
         (header-size 0))
    (%qpack-validate-context context)
    (when stream-id
      (%qpack-nonnegative-integer stream-id :stream-id))
    (unless (and (integerp start) (<= 0 start limit))
      (%qpack-error "QPACK header block start is invalid." start))
    (unless (and (integerp limit) (<= start limit (length bytes)))
      (%qpack-error "QPACK header block end is invalid." limit))
    (multiple-value-bind (encoded-required next)
        (%qpack-decode-integer bytes position 8 limit)
      (setf position next)
      (let ((required (%qpack-required-insert-count-decode
                       context encoded-required)))
        (multiple-value-bind (encoded-delta base-position)
            (%qpack-decode-integer bytes position 7 limit)
          (let ((base (if (logbitp 7 (aref bytes position))
                          (- required encoded-delta 1)
                          (+ required encoded-delta))))
            (when (< base 0)
              (%qpack-error "QPACK field block base is negative." base))
            (when (> required (qpack-context-insert-count context))
              (error 'http-qpack-blocked
                     :message "QPACK header block is blocked on dynamic inserts."
                     :operation :qpack
                     :required-insert-count required
                     :available-insert-count
                     (qpack-context-insert-count context)
                     :stream-id stream-id))
            (setf position base-position)
            (loop while (< position limit)
                  do (multiple-value-bind (header next)
                         (%qpack-decode-field
                          context bytes position base limit)
                       (incf header-size (%qpack-field-header-size header))
                       (http-check-size-limit
                        header-size
                        (qpack-context-max-header-list-size context)
                        :header-list)
                       (push header fields)
                       (setf position next)))
            (values (nreverse fields) position required base)))))))
