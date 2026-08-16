(in-package #:http-message-kit/test)

(defvar *fixture-phase* nil)

(cl-weave:defmatcher :to-have-http-status (response expected)
  "Checks that a response has the requested HTTP status."
  (let ((actual-status (http-response-status response))
        (wanted-status (first expected)))
    (values (= actual-status wanted-status)
            actual-status
            wanted-status)))

(describe-each ((:information 199)
                (:success 200)
                (:redirect 302)
                (:client-error 404)
                (:server-error 500))
    "status class ~A"
    (class status)
  (let ((predicate (ecase class
                     (:information #'http-information-status-p)
                     (:success #'http-success-status-p)
                     (:redirect #'http-redirection-status-p)
                     (:client-error #'http-client-error-status-p)
                     (:server-error #'http-server-error-status-p))))
    (it "recognizes the status class"
      (expect (funcall predicate status) :to-be-truthy))
    (it "stores the status in a response"
      (expect (make-http-response :status status)
              :to-have-http-status status))))

(describe "abstract message contracts"
  (before-each
    (setf *fixture-phase* :before))
  (around-each (next)
    (let ((*fixture-phase* :around))
      (funcall next)))
  (after-each
    (setf *fixture-phase* :after))
  (it-each ((:get "GET")
            (:post "POST"))
      "preserves method token case ~A"
      (label method)
    (declare (ignore label))
    (expect (http-request-method
             (make-http-request :method method
                                :uri "https://example.test/"))
            :to-equalp method))
  (it "runs fixture hooks around the test body"
    (expect *fixture-phase* :to-be :around))
  (it-property "status predicates partition valid statuses"
      ((status (gen-integer :min 100 :max 599)))
    (let ((matches (count-if #'identity
                             (list (http-information-status-p status)
                                   (http-success-status-p status)
                                   (http-redirection-status-p status)
                                   (http-client-error-status-p status)
                                   (http-server-error-status-p status)))))
      (expect matches :to-be 1))))

(describe "body CPS"
  (it "passes chunks through continuations and returns completion values"
    (let ((chunks (list (octets 1 2) (octets 3) nil))
          (seen nil))
      (with-continuation-values (result next calledp)
        (http-body-cps
         (lambda () (pop chunks))
         (lambda (chunk continuation)
           (push chunk seen)
           (funcall continuation))
         (lambda ()
           (funcall #'next :complete (length seen))))
        (expect calledp :to-be-truthy)
        (expect result :to-equal '(:complete 2))
        (expect (nreverse seen) :to-equalp (list (octets 1 2) (octets 3))))))
  (it "copies and validates chunks before invoking the continuation"
    (let* ((original (octets 1 2))
           (chunks (list original nil))
           (seen nil))
      (http-body-cps
       (lambda () (pop chunks))
       (lambda (chunk continuation)
         (setf (aref chunk 0) 9)
         (setf seen chunk)
         (funcall continuation))
       (lambda () nil))
      (expect original :to-equalp (octets 1 2))
      (expect seen :to-equalp (octets 9 2)))
    (let ((condition-type nil))
      (http-body-cps
       (lambda () (list 256))
       (lambda (chunk continuation)
         (declare (ignore chunk continuation)))
       (lambda () nil)
       :on-error (lambda (condition)
                   (setf condition-type (type-of condition))))
      (expect condition-type :to-be 'http-protocol-error)))
  (it "routes body errors to the error continuation"
    (let ((condition-type nil))
      (with-continuation-values (result next)
        (http-body-cps
         (lambda () (error 'http-protocol-error
                           :message "broken"
                           :operation :body))
         (lambda (chunk continuation)
           (declare (ignore chunk continuation)))
         (lambda () (funcall #'next :unexpected))
         :on-error (lambda (condition)
                     (setf condition-type (type-of condition))
                     (funcall #'next :handled)))
        (expect result :to-equal '(:handled)))
      (expect condition-type :to-be 'http-protocol-error)))
  (it "re-signals body errors when no error continuation is supplied"
    (signals http-protocol-error
      (http-body-cps
       (lambda ()
         (error 'http-protocol-error
                :message "broken"
                :operation :body))
       (lambda (chunk continuation)
         (declare (ignore chunk continuation)))
       (lambda () nil))))
  (it "requires function callbacks"
    (dolist (callbacks
              (list (list nil (lambda (chunk continuation)
                                (declare (ignore chunk continuation)))
                          (lambda () nil))
                    (list (lambda () nil) nil (lambda () nil))
                    (list (lambda () nil)
                          (lambda (chunk continuation)
                            (declare (ignore chunk continuation)))
                          nil)
                    (list (lambda () nil)
                          (lambda (chunk continuation)
                            (declare (ignore chunk continuation)))
                          (lambda () nil)
                          :on-error t)))
      (signals http-protocol-error
        (apply #'http-body-cps callbacks)))))
