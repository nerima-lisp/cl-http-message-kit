(in-package #:http-message-kit)

(defstruct (hpack-entry (:constructor %make-hpack-entry (name value size)))
  name
  value
  size)

(defstruct hpack-huffman-node
  symbol
  zero
  one)

(defstruct (hpack-context
            (:constructor %make-hpack-context
                (max-table-size table-capacity dynamic-table dynamic-size
                 pending-table-size max-header-list-size huffman-p))
            (:conc-name hpack-context-))
  max-table-size
  table-capacity
  dynamic-table
  dynamic-size
  pending-table-size
  max-header-list-size
  huffman-p)

(defun %hpack-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :hpack
         :detail detail))

(defun %hpack-octets (value)
  (handler-case
      (%copy-octets value :allow-list t)
    (http-protocol-error ()
      (%hpack-error "HPACK input must be a one-dimensional octet sequence."
                    (type-of value)))))

(defun %hpack-validate-context (context)
  (unless (hpack-context-p context)
    (%hpack-error "HPACK context is invalid." (type-of context)))
  context)

(defun %hpack-nonnegative-integer (value name)
  (unless (and (integerp value) (>= value 0))
    (%hpack-error (format nil "~A must be a non-negative integer." name)
                  value))
  value)

(defun make-hpack-context (&key (max-table-size 4096)
                                (max-header-list-size *default-max-header-bytes*)
                                (huffman t))
  (%hpack-nonnegative-integer max-table-size :max-table-size)
  (unless (or (null max-header-list-size)
              (and (integerp max-header-list-size)
                   (>= max-header-list-size 0)))
    (%hpack-error "MAX_HEADER_LIST_SIZE must be a non-negative integer or NIL."
                  max-header-list-size))
  (unless (member huffman '(nil t))
    (%hpack-error "HPACK Huffman selection must be boolean." huffman))
  (%make-hpack-context max-table-size max-table-size nil 0 nil
                       max-header-list-size huffman))

(defun hpack-context-table-size (context)
  (%hpack-validate-context context)
  (hpack-context-dynamic-size context))

(defun hpack-context-dynamic-entry-count (context)
  (%hpack-validate-context context)
  (length (hpack-context-dynamic-table context)))

(defun %hpack-clear-table (context)
  (setf (hpack-context-dynamic-table context) nil
        (hpack-context-dynamic-size context) 0)
  context)

(defun %hpack-evict-to (context limit)
  (loop for oldest = (car (last (hpack-context-dynamic-table context)))
        while (> (hpack-context-dynamic-size context) limit)
        do (unless oldest
             (%hpack-error "HPACK dynamic table accounting is inconsistent."))
           (setf (hpack-context-dynamic-table context)
                 (butlast (hpack-context-dynamic-table context)))
           (decf (hpack-context-dynamic-size context)
                 (hpack-entry-size oldest)))
  context)

(defun hpack-context-set-max-table-size (context size)
  (%hpack-validate-context context)
  (%hpack-nonnegative-integer size :max-table-size)
  (setf (hpack-context-max-table-size context) size
        (hpack-context-table-capacity context) size
        (hpack-context-pending-table-size context) size)
  (%hpack-evict-to context size)
  context)

(defun %hpack-entry-size (name value)
  (+ 32 (length (%octets-from-string name))
     (length (%octets-from-string value))))

(defun %hpack-add-entry (context name value)
  (let ((size (%hpack-entry-size name value))
        (capacity (hpack-context-table-capacity context)))
    (if (> size capacity)
        (%hpack-clear-table context)
        (progn
          (%hpack-evict-to context (- capacity size))
          (push (%make-hpack-entry name value size)
                (hpack-context-dynamic-table context))
          (incf (hpack-context-dynamic-size context) size))))
  context)

(defun %hpack-static-entry (index)
  (when (and (integerp index)
             (<= 1 index)
             (<= index (length *hpack-static-table*)))
    (let ((entry (aref *hpack-static-table* (1- index))))
      (values (first entry) (second entry)))))

(defun %hpack-index-entry (context index)
  (%hpack-nonnegative-integer index :index)
  (when (zerop index)
    (%hpack-error "HPACK index zero is invalid."))
  (multiple-value-bind (name value) (%hpack-static-entry index)
    (if name
        (values name value)
        (let* ((dynamic-index (- index (length *hpack-static-table*) 1))
               (entry (and (>= dynamic-index 0)
                           (< dynamic-index
                              (length (hpack-context-dynamic-table context))))))
          (unless entry
            (%hpack-error "HPACK index is outside the current table." index))
          (let ((item (nth dynamic-index
                           (hpack-context-dynamic-table context))))
            (values (hpack-entry-name item)
                    (hpack-entry-value item)))))))

(defun %hpack-find-exact-index (context name value)
  (or (loop for index from 1 to (length *hpack-static-table*)
            for entry = (aref *hpack-static-table* (1- index))
            when (and (string= name (first entry))
                      (string= value (second entry)))
              do (return index))
      (loop for entry in (hpack-context-dynamic-table context)
            for index from (1+ (length *hpack-static-table*))
            when (and (string= name (hpack-entry-name entry))
                      (string= value (hpack-entry-value entry)))
              do (return index))))

(defun %hpack-find-name-index (context name)
  (or (loop for index from 1 to (length *hpack-static-table*)
            for entry = (aref *hpack-static-table* (1- index))
            when (string= name (first entry))
              do (return index))
      (loop for entry in (hpack-context-dynamic-table context)
            for index from (1+ (length *hpack-static-table*))
            when (string= name (hpack-entry-name entry))
              do (return index))))

(defun hpack-encode-integer (integer prefix-bits &optional (prefix-value 0))
  (%hpack-nonnegative-integer integer :integer)
  (unless (and (integerp prefix-bits) (<= 1 prefix-bits 8))
    (%hpack-error "HPACK integer prefix width must be between one and eight."
                  prefix-bits))
  (let ((limit (1- (ash 1 prefix-bits))))
    (unless (and (integerp prefix-value)
                 (<= 0 prefix-value)
                 (< prefix-value 256)
                 (zerop (logand prefix-value limit)))
      (%hpack-error "HPACK integer prefix value overlaps the integer bits."
                    prefix-value))
    (if (< integer limit)
        (vector (logior prefix-value integer))
        (let ((bytes (list (logior prefix-value limit)))
              (remaining (- integer limit)))
          (loop while (>= remaining 128)
                do (push (logior #x80 (logand remaining #x7f)) bytes)
                   (setf remaining (floor remaining 128)))
          (push remaining bytes)
          (coerce (nreverse bytes)
                  '(vector (unsigned-byte 8)))))))

(defun hpack-decode-integer (octets &optional (start 0) (prefix-bits 8) end)
  (let* ((bytes (%hpack-octets octets))
         (limit (if (null end) (length bytes) end)))
    (unless (and (integerp start) (<= 0 start))
      (%hpack-error "HPACK integer start is outside the input." start))
    (unless (and (integerp limit) (<= start limit (length bytes)))
      (%hpack-error "HPACK integer end is outside the input." limit))
    (unless (and (integerp prefix-bits) (<= 1 prefix-bits 8))
      (%hpack-error "HPACK integer prefix width must be between one and eight."
                    prefix-bits))
    (when (= start limit)
      (%hpack-error "HPACK integer is truncated."))
    (let* ((mask (1- (ash 1 prefix-bits)))
           (first (aref bytes start))
           (initial (logand first mask)))
      (if (< initial mask)
          (values initial (1+ start))
          (let ((value mask)
                (multiplier 1)
                (position (1+ start)))
            (loop
              (when (= position limit)
                (%hpack-error "HPACK integer continuation is truncated."))
              (let ((byte (aref bytes position)))
                (incf value (* (logand byte #x7f) multiplier))
                (if (< byte 128)
                    (return (values value (1+ position)))
                    (progn
                      (setf multiplier (* multiplier 128))
                      (when (> multiplier (ash 1 56))
                        (%hpack-error "HPACK integer is too large.")))))
              (incf position)))))))

(defun %hpack-huffman-build-tree ()
  (let ((root (make-hpack-huffman-node)))
    (loop for symbol from 0 below (length *hpack-huffman-codes*)
          for pair = (aref *hpack-huffman-codes* symbol)
          for code = (car pair)
          for bits = (cdr pair)
          do (let ((node root))
               (loop for bit from (1- bits) downto 0
                     for one = (logbitp bit code)
                     do (when (hpack-huffman-node-symbol node)
                          (%hpack-error "HPACK Huffman table has a prefix collision."))
                        (if one
                            (unless (hpack-huffman-node-one node)
                              (setf (hpack-huffman-node-one node)
                                    (make-hpack-huffman-node)))
                            (unless (hpack-huffman-node-zero node)
                              (setf (hpack-huffman-node-zero node)
                                    (make-hpack-huffman-node))))
                        (setf node (if one
                                       (hpack-huffman-node-one node)
                                       (hpack-huffman-node-zero node))))
               (when (or (hpack-huffman-node-symbol node)
                         (hpack-huffman-node-zero node)
                         (hpack-huffman-node-one node))
                 (%hpack-error "HPACK Huffman table contains a duplicate code."))
               (setf (hpack-huffman-node-symbol node) symbol)))
    root))

(defvar *hpack-huffman-root* nil)

(defun %hpack-huffman-root ()
  (or *hpack-huffman-root*
      (setf *hpack-huffman-root* (%hpack-huffman-build-tree))))

(defun %hpack-huffman-encode (octets)
  (let ((buffer 0)
        (bits 0)
        (bytes nil))
    (loop for octet across octets
          for pair = (aref *hpack-huffman-codes* octet)
          do (setf buffer (logior (ash buffer (cdr pair)) (car pair))
                   bits (+ bits (cdr pair)))
             (loop while (>= bits 8)
                   do (decf bits 8)
                      (push (ldb (byte 8 bits) buffer) bytes)
                      (setf buffer (logand buffer
                                           (1- (ash 1 bits))))))
    (when (plusp bits)
      (push (logior (ash buffer (- 8 bits))
                    (1- (ash 1 (- 8 bits))))
            bytes))
    (coerce (nreverse bytes) '(vector (unsigned-byte 8)))))

(defun %hpack-huffman-decode (octets)
  (let* ((root (%hpack-huffman-root))
         (node root)
         (buffer 0)
         (padding-bits 0)
         (result nil))
    (loop for octet across octets
          do (loop for bit from 7 downto 0
                   for one = (logbitp bit octet)
                   do (setf node (if one
                                    (hpack-huffman-node-one node)
                                    (hpack-huffman-node-zero node)))
                      (unless node
                        (%hpack-error "HPACK Huffman code is invalid."))
                      (if (hpack-huffman-node-symbol node)
                          (progn
                            (when (= (hpack-huffman-node-symbol node) 256)
                              (%hpack-error "HPACK Huffman EOS cannot appear in a string."))
                            (push (hpack-huffman-node-symbol node) result)
                            (setf node root
                                  buffer 0
                                  padding-bits 0))
                          (progn
                            (setf buffer (logior (ash buffer 1)
                                                 (if one 1 0)))
                            (incf padding-bits)))))
    (when (plusp padding-bits)
      (unless (and (<= padding-bits 7)
                   (= buffer (1- (ash 1 padding-bits))))
        (%hpack-error "HPACK Huffman padding is invalid.")))
    (coerce (nreverse result) '(vector (unsigned-byte 8)))))

(defun hpack-encode-string (string &key (huffman nil))
  (unless (stringp string)
    (%hpack-error "HPACK strings must be strings." (type-of string)))
  (unless (member huffman '(nil t))
    (%hpack-error "HPACK Huffman selection must be boolean." huffman))
  (let* ((plain (%octets-from-string string))
         (payload (if huffman
                      (%hpack-huffman-encode plain)
                      plain))
         (length-prefix (hpack-encode-integer (length payload) 7
                                              (if huffman #x80 0))))
    (%append-octets length-prefix payload)))

(defun hpack-decode-string (octets &optional (start 0) end)
  (let* ((bytes (%hpack-octets octets))
         (limit (if (null end) (length bytes) end)))
    (unless (and (integerp start) (<= 0 start))
      (%hpack-error "HPACK string start is outside the input." start))
    (unless (and (integerp limit) (<= start limit (length bytes)))
      (%hpack-error "HPACK string end is outside the input." limit))
    (unless (< start limit)
      (%hpack-error "HPACK string is truncated."))
    (let ((huffman (logbitp 7 (aref bytes start))))
      (multiple-value-bind (length position)
          (hpack-decode-integer bytes start 7 limit)
        (let ((payload-end (+ position length)))
          (when (> payload-end limit)
            (%hpack-error "HPACK string payload is truncated."))
          (let ((payload (subseq bytes position payload-end)))
            (values (if huffman
                        (%string-from-octets (%hpack-huffman-decode payload))
                        (%string-from-octets payload))
                    payload-end)))))))

(defun %hpack-header-size (header)
  (+ 32 (length (%octets-from-string (%header-name header)))
     (length (%octets-from-string (%header-content header)))))

(defun %hpack-header-list (headers)
  (unless (%proper-list-p headers)
    (%hpack-error "HPACK headers must be a proper list."))
  (%normalize-headers headers))

(defun %hpack-validate-header-name (name)
  (unless (or (%header-name-p name)
              (%pseudo-header-name-p name))
    (%hpack-error "HPACK header name is invalid." name))
  (when (some (lambda (character)
                (let ((code (char-code character)))
                  (and (<= (char-code #\A) code)
                       (<= code (char-code #\Z)))))
              name)
    (%hpack-error "HTTP/2 and HTTP/3 header names must be lowercase." name))
  name)

(defun %hpack-encode-literal (context header mode huffman)
  (let* ((name (%hpack-validate-header-name (%header-name header)))
         (value (%header-content header))
         (name-index (and (not (zerop (length name)))
                          (%hpack-find-name-index context name)))
         (prefix-bits (ecase mode
                        (:incremental 6)
                        (:without-indexing 4)
                        (:never 4)))
         (prefix-value (ecase mode
                         (:incremental #x40)
                         (:without-indexing 0)
                         (:never #x10)))
         (parts (list (hpack-encode-integer (or name-index 0)
                                            prefix-bits prefix-value))))
    (unless name-index
      (push (hpack-encode-string name :huffman huffman) parts))
    (push (hpack-encode-string value :huffman huffman) parts)
    (when (eq mode :incremental)
      (%hpack-add-entry context name value))
    (apply #'%append-octets (nreverse parts))))

(defun %hpack-indexing-mode (header indexing)
  (case indexing
    (:auto (if (%sensitive-header-p (%header-name header))
               :never
               :auto))
    (:incremental :incremental)
    (:without-indexing :without-indexing)
    (:never :never)
    (:indexed :indexed)
    (otherwise
     (%hpack-error "Unknown HPACK indexing mode." indexing))))

(defun hpack-encode-header-block (headers &key context (indexing :auto)
                                           (huffman nil huffman-supplied-p))
  (let ((context (or context (make-hpack-context)))
        (parts nil))
    (%hpack-validate-context context)
    (unless huffman-supplied-p
      (setf huffman (hpack-context-huffman-p context)))
    (unless (member huffman '(nil t))
      (%hpack-error "HPACK Huffman selection must be boolean." huffman))
    (when (hpack-context-pending-table-size context)
      (push (hpack-encode-integer
             (hpack-context-pending-table-size context) 5 #x20)
            parts)
      (setf (hpack-context-pending-table-size context) nil))
    (dolist (header (%hpack-header-list headers))
      (%hpack-validate-header-name (%header-name header))
      (let ((mode (%hpack-indexing-mode header indexing)))
        (cond
          ((eq mode :auto)
           (let ((index (%hpack-find-exact-index
                         context (%header-name header)
                         (%header-content header))))
             (if (and index
                      (not (%sensitive-header-p (%header-name header))))
                 (push (hpack-encode-integer index 7 #x80) parts)
                 (push (%hpack-encode-literal
                        context header
                        (if (%sensitive-header-p (%header-name header))
                            :never
                            :incremental)
                        huffman)
                       parts))))
          ((eq mode :indexed)
           (let ((index (%hpack-find-exact-index
                         context (%header-name header)
                         (%header-content header))))
             (unless index
               (%hpack-error "Indexed HPACK representation has no matching table entry."
                             header))
             (push (hpack-encode-integer index 7 #x80) parts)))
          (t
           (push (%hpack-encode-literal context header mode huffman) parts)))))
    (apply #'%append-octets (nreverse parts))))

(defun %hpack-read-field (context bytes position prefix-bits limit)
  (multiple-value-bind (index next)
      (hpack-decode-integer bytes position prefix-bits limit)
    (if (zerop index)
        (multiple-value-bind (name name-end)
            (hpack-decode-string bytes next limit)
          (values name name-end))
        (multiple-value-bind (name value) (%hpack-index-entry context index)
          (declare (ignore value))
          (values name next)))))

(defun %hpack-read-literal (context bytes position prefix-bits mode limit)
  (multiple-value-bind (name name-end)
      (%hpack-read-field context bytes position prefix-bits limit)
    (multiple-value-bind (value value-end)
        (hpack-decode-string bytes name-end limit)
      (when (eq mode :incremental)
        (%hpack-add-entry context name value))
      (values (make-http-header name value
                                :pseudo-p (%pseudo-header-name-p name))
              value-end))))

(defun hpack-decode-header-block (octets &key context (start 0) end)
  (let* ((context (or context (make-hpack-context)))
         (bytes (%hpack-octets octets))
         (limit (if (null end) (length bytes) end))
         (position start)
         (fields nil)
         (saw-field nil)
         (header-size 0))
    (%hpack-validate-context context)
    (unless (and (integerp start) (<= 0 start limit))
      (%hpack-error "HPACK header block start is invalid." start))
    (unless (and (integerp limit) (<= start limit (length bytes)))
      (%hpack-error "HPACK header block end is invalid." limit))
    (loop while (< position limit)
          do (let ((first (aref bytes position)))
               (cond
                 ((logbitp 7 first)
                  (setf saw-field t)
                  (multiple-value-bind (index next)
                      (hpack-decode-integer bytes position 7 limit)
                    (multiple-value-bind (name value)
                        (%hpack-index-entry context index)
                      (let ((header (make-http-header
                                     name value
                                     :pseudo-p (%pseudo-header-name-p name))))
                        (incf header-size (%hpack-header-size header))
                        (http-check-size-limit
                         header-size
                         (hpack-context-max-header-list-size context)
                         :header-list)
                        (push header fields)
                        (setf position next)))))
                 ((= (logand first #xE0) #x20)
                  (when saw-field
                    (%hpack-error "HPACK table-size updates must precede header fields."))
                  (multiple-value-bind (size next)
                      (hpack-decode-integer bytes position 5 limit)
                    (when (> size (hpack-context-max-table-size context))
                      (%hpack-error
                       "HPACK table-size update exceeds the configured maximum." size))
                    (setf (hpack-context-table-capacity context) size)
                    (%hpack-evict-to context size)
                    (setf position next)))
                 ((= (logand first #xC0) #x40)
                  (setf saw-field t)
                  (multiple-value-bind (header next)
                      (%hpack-read-literal context bytes position 6 :incremental
                                           limit)
                    (incf header-size (%hpack-header-size header))
                    (http-check-size-limit
                     header-size
                     (hpack-context-max-header-list-size context)
                     :header-list)
                    (push header fields)
                    (setf position next)))
                 ((= (logand first #xF0) #x10)
                  (setf saw-field t)
                  (multiple-value-bind (header next)
                      (%hpack-read-literal context bytes position 4 :never limit)
                    (incf header-size (%hpack-header-size header))
                    (http-check-size-limit
                     header-size
                     (hpack-context-max-header-list-size context)
                     :header-list)
                    (push header fields)
                    (setf position next)))
                 (t
                  (setf saw-field t)
                  (multiple-value-bind (header next)
                      (%hpack-read-literal context bytes position 4
                                           :without-indexing limit)
                    (incf header-size (%hpack-header-size header))
                    (http-check-size-limit
                     header-size
                     (hpack-context-max-header-list-size context)
                     :header-list)
                    (push header fields)
                    (setf position next))))))
    (values (nreverse fields) position)))
