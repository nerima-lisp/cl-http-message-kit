(in-package #:http-message-kit)

(defun %monotonic-time ()
  (/ (float (get-internal-real-time))
     (float internal-time-units-per-second)))

(defun %validate-clock-function (clock-function)
  (unless (functionp clock-function)
    (error 'http-protocol-error
           :message "HTTP deadline clocks must be functions."
           :operation :deadline
           :detail (type-of clock-function)))
  clock-function)

(defun %clock-reading (clock-function)
  (let ((reading (funcall (%validate-clock-function clock-function))))
    (unless (realp reading)
      (error 'http-protocol-error
             :message "HTTP deadline clocks must return real seconds."
             :operation :deadline
             :detail (type-of reading)))
    reading))

(defun http-deadline (timeout &key deadline (clock-function #'%monotonic-time))
  "Return an absolute monotonic deadline from TIMEOUT and DEADLINE.
Both values are seconds.  NIL means that no bound was requested."
  (%validate-clock-function clock-function)
  (unless (or (null timeout) (and (realp timeout) (>= timeout 0)))
    (error 'http-protocol-error
           :message "HTTP timeout must be a non-negative real number."
           :operation :deadline
           :detail timeout))
  (unless (or (null deadline) (realp deadline))
    (error 'http-protocol-error
           :message "HTTP deadline must be a real number."
           :operation :deadline
           :detail deadline))
  (let ((timeout-deadline (and timeout (+ (%clock-reading clock-function) timeout))))
    (cond ((and timeout-deadline deadline) (min timeout-deadline deadline))
          (timeout-deadline timeout-deadline)
          (deadline deadline)
          (t nil))))

(defun %check-deadline (deadline clock-function &optional (kind :read))
  (when (and deadline (>= (%clock-reading clock-function) deadline))
    (error 'http-timeout
           :message "The HTTP operation exceeded its deadline."
           :operation kind
           :kind kind)))

(defun http-check-size-limit (observed limit &optional (kind :body))
  "Signal HTTP-SIZE-LIMIT-EXCEEDED when OBSERVED exceeds LIMIT.
LIMIT may be NIL to disable the check.  Return OBSERVED when it is within
the configured bound."
  (unless (and (integerp observed) (>= observed 0))
    (error 'http-protocol-error
           :message "Observed HTTP size must be a non-negative integer."
           :operation :size-limit
           :detail observed))
  (unless (or (null limit) (and (integerp limit) (>= limit 0)))
    (error 'http-protocol-error
           :message "HTTP size limits must be non-negative integers or NIL."
           :operation :size-limit
           :detail limit))
  (when (and limit (> observed limit))
    (error 'http-size-limit-exceeded
           :message "The HTTP message exceeded its configured size limit."
           :operation :size-limit
           :limit limit
           :observed observed
           :kind kind))
  observed)

(defun %proper-list-p (value)
  (loop with seen = nil
        for tail = value then (cdr tail)
        while (consp tail)
        do (when (member tail seen :test #'eq)
             (return nil))
           (push tail seen)
        finally (return (null tail))))

(defun %copy-octets (value &key (allow-list t))
  (cond
    ((and (arrayp value) (= (array-rank value) 1)
          (not (stringp value))
          (loop for item across value
                always (and (integerp item) (<= 0 item 255))))
     (let ((copy (make-array (length value) :element-type '(unsigned-byte 8))))
       (replace copy value)
       copy))
    ((and allow-list (listp value))
     (unless (%proper-list-p value)
       (error 'http-protocol-error
              :message "HTTP message bodies must be one-dimensional octet vectors."
              :operation :body
              :detail (type-of value)))
     (if (every (lambda (item)
                  (and (integerp item) (<= 0 item 255)))
                value)
         (make-array (length value) :element-type '(unsigned-byte 8)
                     :initial-contents value)
         (error 'http-protocol-error
                :message "HTTP message bodies must be one-dimensional octet vectors."
                :operation :body
                :detail (type-of value))))
    (t
     (error 'http-protocol-error
            :message "HTTP message bodies must be one-dimensional octet vectors."
            :operation :body
            :detail (type-of value)))))

(defun %empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun %octets-from-string (string &key (ascii-only nil))
  (unless (stringp string)
    (error 'http-protocol-error
           :message "HTTP wire text must be a string."
           :operation :octet-encoding
           :detail (type-of string)))
  (let ((octets (make-array (length string)
                            :element-type '(unsigned-byte 8))))
    (loop for character across string
          for index from 0
          for code = (char-code character)
          do (when (or (> code #xff)
                       (and ascii-only (> code #x7f)))
               (error 'http-protocol-error
                      :message "HTTP wire text contains a character that cannot be encoded."
                      :operation :octet-encoding
                      :detail character))
             (setf (aref octets index) code))
    octets))

(defun %string-from-octets (octets)
  (let ((copy (%copy-octets octets :allow-list nil)))
    (map 'string #'code-char copy)))

(defun %append-octets (&rest sequences)
  (let ((length (reduce #'+ sequences :key #'length :initial-value 0)))
    (let ((result (make-array length :element-type '(unsigned-byte 8)))
          (position 0))
      (dolist (sequence sequences result)
        (replace result sequence :start1 position)
        (incf position (length sequence))))))

(defun %octet-vector-p (value)
  (and (arrayp value)
       (= (array-rank value) 1)
       (not (stringp value))
       (equal (array-element-type value) '(unsigned-byte 8))))

(defun %ascii-lowercase (string)
  (map 'string
       (lambda (character)
         (let ((code (char-code character)))
           (if (and (<= (char-code #\A) code)
                    (<= code (char-code #\Z)))
               (code-char (+ code (- (char-code #\a) (char-code #\A))))
               character)))
       string))

(defun %ascii-name-char-p (character)
  (let ((code (char-code character)))
    (or (and (<= (char-code #\0) code)
             (<= code (char-code #\9)))
        (and (<= (char-code #\A) code)
             (<= code (char-code #\Z)))
        (and (<= (char-code #\a) code)
             (<= code (char-code #\z)))
        (and (<= code #x7f)
             (find character "!#$%&'*+-.^_`|~" :test #'char=)))))

(defun %token-p (string)
  (and (plusp (length string))
       (every #'%ascii-name-char-p string)))

(defun %header-name-p (string)
  (%token-p string))

(defun %header-value-p (string)
  (every (lambda (character)
           (let ((code (char-code character)))
             (or (= code #x09)
                 (<= #x20 code #x7e)
                 (<= #x80 code #xff))))
         string))

(defun %trim-ows (string)
  (string-trim '(#\Space #\Tab) string))

(defun %decimal-string-p (string)
  (and (plusp (length string))
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (<= (char-code #\0) code)
                       (<= code (char-code #\9)))))
              string)))

(defun %parse-decimal (string)
  (unless (%decimal-string-p string)
    (error 'http-protocol-error
           :message "Expected an ASCII decimal integer."
           :operation :integer
           :detail string))
  (parse-integer string))

(defun %bounded-diagnostic (value &optional (limit 160))
  (let ((text (princ-to-string value)))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit) "...")
        text)))
