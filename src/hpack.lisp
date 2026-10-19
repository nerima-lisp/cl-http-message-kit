(in-package #:http-message-kit)

(progn
(defconstant +hpack-codec-maximum-integer+ #xffffffff)
(defconstant +hpack-default-maximum-block-size+ (* 16 1024 1024))
(defconstant +hpack-default-maximum-string-length+ (* 16 1024 1024))

(defstruct (hpack-context
            (:constructor %make-hpack-context
                (max-table-size table-capacity max-header-list-size
                 max-field-count max-block-size max-string-length huffman-p
                 validate-header-names codec-table encoder decoder)))
  max-table-size
  table-capacity
  max-header-list-size
  max-field-count
  max-block-size
  max-string-length
  huffman-p
  validate-header-names
  codec-table
  encoder
  decoder)

(defun %hpack-error (message &optional detail)
  (error 'http-protocol-error
         :message message
         :operation :hpack
         :detail detail))

(defun %hpack-codec-call (function &rest arguments)
  (handler-case
      (apply function arguments)
    (hpack-codec:hpack-codec-error (condition)
      (%hpack-error
       (hpack-codec:hpack-codec-error-message condition)
       (list :codec-operation
             (hpack-codec:hpack-codec-error-operation condition)
             :codec-detail
             (hpack-codec:hpack-codec-error-detail condition))))))

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

(defun %hpack-validate-optional-limit (value name)
  (unless (or (null value)
              (and (integerp value) (>= value 0)))
    (%hpack-error (format nil "~A must be a non-negative integer or NIL." name)
                  value))
  value)

(defun make-hpack-context (&key (max-table-size 4096)
                                (max-header-list-size *default-max-header-bytes*)
                                (max-field-count 65536)
                                (max-block-size +hpack-default-maximum-block-size+)
                                (max-string-length
                                  +hpack-default-maximum-string-length+)
                                (validate-header-names nil)
                                (huffman t))
  (%hpack-nonnegative-integer max-table-size :max-table-size)
  (%hpack-validate-optional-limit max-header-list-size :max-header-list-size)
  (%hpack-nonnegative-integer max-field-count :max-field-count)
  (%hpack-nonnegative-integer max-block-size :max-block-size)
  (%hpack-nonnegative-integer max-string-length :max-string-length)
  (unless (member validate-header-names '(nil t) :test #'eq)
    (%hpack-error "HPACK header-name validation must be boolean."
                  validate-header-names))
  (unless (member huffman '(nil t) :test #'eq)
    (%hpack-error "HPACK Huffman selection must be boolean." huffman))
  (let* ((header-list-limit (or max-header-list-size
                                +hpack-codec-maximum-integer+))
         (encoder
           (%hpack-codec-call
            #'hpack-codec:make-hpack-encoder
            :maximum-table-size max-table-size
            :maximum-block-size max-block-size
            :maximum-header-list-size header-list-limit
            :maximum-field-count max-field-count
            :maximum-string-length max-string-length
            :huffman huffman))
         (decoder
           (%hpack-codec-call
            #'hpack-codec:make-hpack-decoder
            :maximum-table-size max-table-size
            :maximum-block-size max-block-size
            :maximum-header-list-size header-list-limit
            :maximum-field-count max-field-count
            :maximum-string-length max-string-length
            :validate-header-names validate-header-names))
         (table (hpack-codec:hpack-encoder-table encoder)))
    (setf (hpack-codec:hpack-decoder-table decoder) table)
    (%make-hpack-context max-table-size max-table-size max-header-list-size
                         max-field-count max-block-size max-string-length
                         huffman validate-header-names table encoder decoder)))

(defun %hpack-sync-table (context table)
  (setf (hpack-context-codec-table context) table
        (hpack-codec:hpack-encoder-table (hpack-context-encoder context)) table
        (hpack-codec:hpack-decoder-table (hpack-context-decoder context)) table
        (hpack-context-table-capacity context)
        (hpack-codec:hpack-table-maximum-size table))
  context)

(defun hpack-context-table-size (context)
  (%hpack-validate-context context)
  (hpack-codec:hpack-table-size (hpack-context-codec-table context)))

(defun hpack-context-dynamic-entry-count (context)
  (%hpack-validate-context context)
  (hpack-codec:hpack-table-count (hpack-context-codec-table context)))

(defun hpack-context-set-max-table-size (context size)
  (%hpack-validate-context context)
  (%hpack-nonnegative-integer size :max-table-size)
  (%hpack-codec-call
   #'hpack-codec:hpack-encoder-set-table-size
   (hpack-context-encoder context) size)
  (%hpack-codec-call
   #'hpack-codec:hpack-decoder-set-table-size
   (hpack-context-decoder context) size)
  (%hpack-sync-table
   context (hpack-codec:hpack-encoder-table (hpack-context-encoder context)))
  (setf (hpack-context-max-table-size context) size
        (hpack-context-table-capacity context) size)
  context)

(defun %hpack-validate-header-name (name)
  (unless (hpack-codec:hpack-valid-header-name-p name)
    (%hpack-error "HPACK header name is invalid or not lowercase." name))
  name)

(defun %hpack-indexing-representation (indexing)
  (case indexing
    (:auto :auto)
    (:incremental :incremental-indexing)
    (:without-indexing :without-indexing)
    (:never :never-indexed)
    (:indexed :indexed)
    (otherwise (%hpack-error "Unknown HPACK indexing mode." indexing))))

(defun %hpack-prepare-headers (headers context indexing)
  (unless (%proper-list-p headers)
    (%hpack-error "HPACK headers must be a proper list."))
  (let ((normalized (%normalize-headers headers))
        (total-size 0))
    (http-check-size-limit (length normalized)
                           (hpack-context-max-field-count context)
                           :fields)
    (mapcar
     (lambda (header)
       (let* ((name (%hpack-validate-header-name (%header-name header)))
              (value (%header-content header)))
         (http-check-size-limit (length name)
                                (hpack-context-max-string-length context)
                                :header-name)
         (http-check-size-limit (length value)
                                (hpack-context-max-string-length context)
                                :header-value)
         (let ((name-octets (%octets-from-string name))
               (value-octets (%octets-from-string value)))
           (incf total-size (+ 32 (length name-octets) (length value-octets)))
           (when (hpack-context-max-header-list-size context)
             (http-check-size-limit
              total-size (hpack-context-max-header-list-size context)
              :header-list))
           (hpack-codec:make-hpack-header
            :name name-octets
            :value value-octets
            :representation (%hpack-indexing-representation indexing)))))
     normalized)))

(defun %hpack-sensitive-predicate (indexing predicate)
  (when (or predicate (eq indexing :auto))
    (lambda (name-octets value-octets)
      (or (and (eq indexing :auto)
               (%sensitive-header-p (%string-from-octets name-octets)))
          (and predicate
               (funcall predicate name-octets value-octets))))))

(defun hpack-encode-integer (integer prefix-bits &optional (prefix-value 0))
  (%hpack-nonnegative-integer integer :integer)
  (unless (and (integerp prefix-bits) (<= 1 prefix-bits 8))
    (%hpack-error "HPACK integer prefix width must be between one and eight."
                  prefix-bits))
  (%hpack-codec-call
   #'hpack-codec:hpack-encode-integer integer prefix-bits prefix-value
   :maximum +hpack-codec-maximum-integer+))

(defun %hpack-read-integer-bounded (bytes start prefix-bits limit)
  (when (= start limit)
    (%hpack-error "HPACK integer is truncated."))
  (let* ((window-end (min limit (+ start 6)))
         (window (subseq bytes start window-end)))
    (multiple-value-bind (value next)
        (%hpack-codec-call
         #'hpack-codec:hpack-read-integer window 0 prefix-bits
         :maximum +hpack-codec-maximum-integer+)
      (values value (+ start next)))))

(defun hpack-decode-integer (octets &optional (start 0) (prefix-bits 8) end)
  (let* ((bytes (%hpack-octets octets))
         (limit (if (null end) (length bytes) end)))
    (unless (and (integerp start) (<= 0 start limit))
      (%hpack-error "HPACK integer start is outside the input." start))
    (unless (and (integerp limit) (<= start limit (length bytes)))
      (%hpack-error "HPACK integer end is outside the input." limit))
    (unless (and (integerp prefix-bits) (<= 1 prefix-bits 8))
      (%hpack-error "HPACK integer prefix width must be between one and eight."
                    prefix-bits))
    (%hpack-read-integer-bounded bytes start prefix-bits limit)))

(defun hpack-encode-string (string &key (huffman nil))
  (unless (stringp string)
    (%hpack-error "HPACK strings must be strings." (type-of string)))
  (unless (member huffman '(nil t) :test #'eq)
    (%hpack-error "HPACK Huffman selection must be boolean." huffman))
  (http-check-size-limit (length string)
                         +hpack-default-maximum-string-length+
                         :header-string)
  (%hpack-codec-call
   #'hpack-codec:hpack-encode-string (%octets-from-string string)
   :huffman huffman
   :maximum-length +hpack-default-maximum-string-length+))

(defun hpack-decode-string (octets &optional (start 0) end)
  (let* ((bytes (%hpack-octets octets))
         (limit (if (null end) (length bytes) end)))
    (unless (and (integerp start) (<= 0 start limit))
      (%hpack-error "HPACK string start is outside the input." start))
    (unless (and (integerp limit) (<= start limit (length bytes)))
      (%hpack-error "HPACK string end is outside the input." limit))
    (multiple-value-bind (length payload-start)
        (%hpack-read-integer-bounded bytes start 7 limit)
      (when (> length +hpack-default-maximum-string-length+)
        (http-check-size-limit
         length +hpack-default-maximum-string-length+ :header-string))
      (let ((payload-end (+ payload-start length)))
        (when (> payload-end limit)
          (%hpack-error "HPACK string payload is truncated." length))
        (multiple-value-bind (value next ignored-huffman)
            (%hpack-codec-call
             #'hpack-codec:hpack-read-string bytes start
             :maximum-length +hpack-default-maximum-string-length+)
          (declare (ignore ignored-huffman))
          (unless (= next payload-end)
            (%hpack-error "HPACK string length is inconsistent." next))
          (values (%string-from-octets value) next))))))

(defun hpack-encode-header-block
    (headers &key context (indexing :auto)
                  (huffman nil huffman-supplied-p)
                  (static-only-p nil)
                  sensitive-predicate)
  (let ((context (or context (make-hpack-context))))
    (%hpack-validate-context context)
    (unless (member indexing '(:auto :incremental :without-indexing :never :indexed)
                    :test #'eq)
      (%hpack-error "Unknown HPACK indexing mode." indexing))
    (unless (member static-only-p '(nil t) :test #'eq)
      (%hpack-error "HPACK static-only selection must be boolean." static-only-p))
    (unless (or (null sensitive-predicate) (functionp sensitive-predicate))
      (%hpack-error "HPACK sensitive predicate must be a function."
                    sensitive-predicate))
    (unless huffman-supplied-p
      (setf huffman (hpack-context-huffman-p context)))
    (unless (member huffman '(nil t) :test #'eq)
      (%hpack-error "HPACK Huffman selection must be boolean." huffman))
    (let* ((codec-headers (%hpack-prepare-headers headers context indexing))
           (predicate (%hpack-sensitive-predicate indexing sensitive-predicate)))
      (if static-only-p
          (let ((wire
                  (%hpack-codec-call
                   #'hpack-codec:hpack-encode-static-header-block
                   codec-headers :huffman huffman
                   :sensitive-predicate predicate)))
            (http-check-size-limit
             (length wire) (hpack-context-max-block-size context)
             :header-block)
            wire)
          (progn
            (%hpack-sync-table context (hpack-context-codec-table context))
            (let ((wire
                    (%hpack-codec-call
                     #'hpack-codec:hpack-encode-header-block
                     (hpack-context-encoder context) codec-headers
                     :huffman huffman :sensitive-predicate predicate)))
              (%hpack-sync-table
               context
               (hpack-codec:hpack-encoder-table
                (hpack-context-encoder context)))
              wire))))))

(defun %hpack-effective-limit (requested requested-p configured name)
  (let ((value (if requested-p requested configured)))
    (%hpack-nonnegative-integer value name)
    (min value configured)))

(defun hpack-decode-header-block
    (octets &key context (start 0) end
                 (max-fields nil max-fields-supplied-p)
                 (max-header-bytes nil max-header-bytes-supplied-p)
                 (validate-header-names nil validate-header-names-supplied-p))
  (let* ((context (or context (make-hpack-context)))
         (bytes (%hpack-octets octets))
         (limit (if (null end) (length bytes) end)))
    (%hpack-validate-context context)
    (when (and validate-header-names-supplied-p
               (not (member validate-header-names '(nil t) :test #'eq)))
      (%hpack-error "HPACK header-name validation must be boolean."
                    validate-header-names))
    (unless (and (integerp start) (<= 0 start limit))
      (%hpack-error "HPACK header block start is invalid." start))
    (unless (and (integerp limit) (<= start limit (length bytes)))
      (%hpack-error "HPACK header block end is invalid." limit))
    (let* ((block-size (- limit start))
           (effective-max-fields
             (%hpack-effective-limit
              max-fields max-fields-supplied-p
              (hpack-context-max-field-count context) :max-fields))
           (configured-header-bytes
             (or (hpack-context-max-header-list-size context)
                 +hpack-codec-maximum-integer+))
           (effective-max-header-bytes
             (%hpack-effective-limit
              max-header-bytes max-header-bytes-supplied-p
              configured-header-bytes :max-header-bytes))
           (validate-names
             (or (hpack-context-validate-header-names context)
                 (and validate-header-names-supplied-p
                      validate-header-names))))
      (unless (member validate-names '(nil t) :test #'eq)
        (%hpack-error "HPACK header-name validation must be boolean."
                      validate-names))
      (http-check-size-limit block-size
                             (hpack-context-max-block-size context)
                             :header-block)
      (%hpack-sync-table context (hpack-context-codec-table context))
      (let* ((block (subseq bytes start limit))
             (codec-headers
               (%hpack-codec-call
                #'hpack-codec:hpack-decode-header-block
                (hpack-context-decoder context) block
                :max-fields effective-max-fields
                :max-header-bytes effective-max-header-bytes
                :validate-header-names validate-names)))
        (%hpack-sync-table
         context
         (hpack-codec:hpack-decoder-table (hpack-context-decoder context)))
        (values
         (mapcar
          (lambda (header)
            (let ((name (%string-from-octets
                         (hpack-codec:hpack-header-name header)))
                  (value (%string-from-octets
                          (hpack-codec:hpack-header-value header))))
              (make-http-header name value
                                :pseudo-p (%pseudo-header-name-p name))))
          codec-headers)
         limit)))))
)
