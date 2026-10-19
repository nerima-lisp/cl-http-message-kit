(in-package #:http-message-kit)

(defun %uri-unreserved-p (character)
  (or (and (char>= character #\A) (char<= character #\Z))
      (and (char>= character #\a) (char<= character #\z))
      (and (char>= character #\0) (char<= character #\9))
      (find character "-._~" :test #'char=)))

(defun %uri-sub-delim-p (character)
  (find character "!$&'()*+,;=" :test #'char=))

(defun %uri-pchar-p (character)
  (or (%uri-unreserved-p character)
      (%uri-sub-delim-p character)
      (find character ":@" :test #'char=)))

(defun %uri-component-character-p (character kind)
  (case kind
    (:host (or (%uri-unreserved-p character)
               (%uri-sub-delim-p character)))
    (:path (or (%uri-pchar-p character)
               (char= character #\/)))
    (:query (or (%uri-pchar-p character)
                (find character "/?" :test #'char=)))
    (otherwise (%uri-pchar-p character))))

(defun %hex-character-p (character)
  (and (characterp character)
       (or (and (char>= character #\0) (char<= character #\9))
           (and (char>= character #\A) (char<= character #\F))
           (and (char>= character #\a) (char<= character #\f)))))

(defun %validate-uri-component (component input kind)
  (loop for index from 0 below (length component)
        for character = (char component index)
        do (cond
             ((char= character #\%)
              (unless (and (< (+ index 2) (length component))
                           (%hex-character-p (char component (1+ index)))
                           (%hex-character-p (char component (+ index 2))))
                (%uri-error input (list kind :bad-percent-escape index))))
             ((not (%uri-component-character-p character kind))
              (%uri-error input (list kind :invalid-character index))))))

(defun %split-on-character (string delimiter)
  (let ((parts '())
        (start 0))
    (loop for position = (position delimiter string :start start)
          do (if position
                 (progn
                   (push (subseq string start position) parts)
                   (setf start (1+ position)))
                 (progn
                   (push (subseq string start) parts)
                   (return (nreverse parts)))))))

(defun %ipv4-address-p (string)
  (let ((parts (%split-on-character string #\.)))
    (and (= (length parts) 4)
         (every (lambda (part)
                 (and (%decimal-string-p part)
                      (or (= (length part) 1)
                          (not (char= (char part 0) #\0)))
                      (<= (%parse-decimal part) 255)))
                parts))))

(defun %dotted-decimal-host-p (string)
  (let ((parts (%split-on-character string #\.)))
    (and (= (length parts) 4)
         (every #'%decimal-string-p parts))))

(defun %ipv6-group-width (group)
  (cond ((find #\. group)
         (and (%ipv4-address-p group) 2))
        ((and (<= 1 (length group) 4)
              (every #'%hex-character-p group))
         1)
        (t nil)))

(defun %ipv6-address-p (address)
  (let* ((double-colon (search "::" address))
         (second-double-colon (and double-colon
                                   (search "::" address
                                           :start2 (+ double-colon 2))))
         (left-text (if double-colon (subseq address 0 double-colon) address))
         (right-text (if double-colon
                         (subseq address (+ double-colon 2))
                         ""))
         (left-groups (if (zerop (length left-text))
                          '()
                          (%split-on-character left-text #\:)))
         (right-groups (if (zerop (length right-text))
                           '()
                           (%split-on-character right-text #\:)))
         (groups (append left-groups right-groups)))
    (and (not second-double-colon)
         (every (lambda (group) (plusp (length group))) groups)
         (every (lambda (group) (%ipv6-group-width group)) groups)
         (loop for group in groups
               for index from 0
               always (or (not (find #\. group))
                          (= index (1- (length groups)))))
         (let ((width (reduce #'+ groups :key #'%ipv6-group-width :initial-value 0)))
           (if double-colon
               (< width 8)
               (= width 8))))))

(defun %ipvfuture-p (literal)
  (let ((dot (position #\. literal :start 1)))
    (and (> (length literal) 3)
         (char-equal (char literal 0) #\v)
         dot
         (> dot 1)
         (< dot (1- (length literal)))
         (every #'%hex-character-p (subseq literal 1 dot))
         (every (lambda (character)
                 (or (%uri-unreserved-p character)
                     (%uri-sub-delim-p character)
                     (char= character #\:)))
                (subseq literal (1+ dot))))))

(defun %ipv6-literal-p (literal)
  (or (%ipv6-address-p literal)
      (%ipvfuture-p literal)))

(defun %authority-port (text start input)
  (let ((port-text (subseq text start)))
    (unless (%decimal-string-p port-text)
      (%uri-error input :port))
    (%parse-decimal port-text)))

(defun %bracketed-authority-parts (authority input)
  (let ((close (position #\] authority)))
    (unless (and close
                 (not (position #\] authority :start (1+ close))))
      (%uri-error input :ipv6-brackets))
    (let ((host (subseq authority 1 close))
          (suffix (subseq authority (1+ close))))
      (unless (%ipv6-literal-p host)
        (%uri-error input :ipv6))
      (cond
        ((zerop (length suffix))
         (values host nil t))
        ((char= (char suffix 0) #\:)
         (when (= (length suffix) 1)
           (%uri-error input :port))
         (values host (%authority-port suffix 1 input) t))
        (t
         (%uri-error input :authority-suffix))))))

(defun %unbracketed-authority-parts (authority input)
  (when (find-if (lambda (character) (find character "[]" :test #'char=))
                 authority)
    (%uri-error input :ipv6-brackets))
  (let ((colon (position #\: authority :from-end t)))
    (when (and colon (position #\: authority :end colon))
      (%uri-error input :unbracketed-ipv6))
    (if colon
        (values (subseq authority 0 colon)
                (%authority-port authority (1+ colon) input)
                nil)
        (values authority nil nil))))

(defun %authority-host-nonempty-p (host input)
  (unless (plusp (length host))
    (%uri-error input :host))
  t)

(defun %authority-host-component-valid-p (host input)
  (when (%dotted-decimal-host-p host)
    (unless (%ipv4-address-p host)
      (%uri-error input :ipv4)))
  (%validate-uri-component host input :host)
  t)

(defun %authority-host-valid-p (host input ipv6-p)
  (%authority-host-nonempty-p host input)
  (unless ipv6-p
    (%authority-host-component-valid-p host input))
  t)

(defun %authority-parts-core (authority input)
  (if (char= (aref authority 0) #\[)
      (%bracketed-authority-parts authority input)
      (%unbracketed-authority-parts authority input)))

(defun %authority-parts-validated (authority input)
  (multiple-value-bind (host port ipv6-p)
      (%authority-parts-core authority input)
    (%authority-host-valid-p host input ipv6-p)
    (when (and port (> port 65535))
      (%uri-error input :port-range))
    (values host port ipv6-p)))

(defun %authority-without-userinfo-p (authority input)
  (when (zerop (length authority))
    (%uri-error input :authority))
  (when (position #\@ authority)
    (%uri-error input :authority))
  t)

(defun %authority-parts (authority input)
  (%authority-without-userinfo-p authority input)
  (multiple-value-bind (host port ipv6-p)
      (%authority-parts-validated authority input)
    (values (%ascii-lowercase host) port ipv6-p)))

(defun %format-uri-authority (host port ipv6-p)
  (let ((authority (if ipv6-p (format nil "[~A]" host) host)))
    (if port
        (format nil "~A:~D" authority port)
        authority)))
