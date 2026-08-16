(in-package #:http-message-kit/test)

(describe "boundary contracts"
  (it "keeps unbounded deadlines and limits explicit"
    (let ((clock (lambda () 100)))
      (expect (http-deadline nil :clock-function clock) :to-be nil)
      (expect (http-deadline nil :deadline 101 :clock-function clock)
              :to-be 101)
      (expect (http-deadline 2 :clock-function clock) :to-be 102)
      (expect (http-check-size-limit 4 nil :body) :to-be 4)))
  (it "evaluates deadline clock expressions once"
    (let ((evaluations 0))
      (with-http-deadline (deadline nil
                           :inherited 8
                           :clock-function
                           (progn
                             (incf evaluations)
                             (lambda () 7)))
        (expect deadline :to-be 8)
        (expect evaluations :to-be 1))))
  (it "rejects malformed bounds and clocks"
    (signals http-protocol-error
      (http-deadline -1 :clock-function (lambda () 1)))
    (signals http-protocol-error
      (http-deadline nil :deadline "later" :clock-function (lambda () 1)))
    (signals http-protocol-error
      (http-deadline nil :clock-function nil))
    (signals http-protocol-error
      (http-deadline 1 :clock-function (lambda () "now")))
    (signals http-protocol-error
      (http-check-size-limit -1 10))
    (signals http-protocol-error
      (http-check-size-limit 1 -1))
    (signals http-protocol-error
      (http-check-size-limit 1 "ten")))
  (it "reports timeout and size-limit metadata"
    (let ((timeout-kind nil)
          (size-limit nil)
          (size-observed nil)
          (size-kind nil))
      (handler-case
          (with-http-deadline (deadline 0
                               :clock-function (lambda () 7)
                               :kind :write)
            deadline)
        (http-timeout (condition)
          (setf timeout-kind (http-timeout-kind condition))))
      (handler-case
          (http-check-size-limit 8 4 :headers)
        (http-size-limit-exceeded (condition)
          (setf size-limit (http-size-limit-exceeded-limit condition)
                size-observed (http-size-limit-exceeded-observed condition)
                size-kind (http-size-limit-exceeded-kind condition))))
      (expect timeout-kind :to-be :write)
      (expect size-limit :to-be 4)
      (expect size-observed :to-be 8)
      (expect size-kind :to-be :headers)))
  (it "rejects cyclic aggregate inputs"
    (let ((header-cycle (cons (list "X-Test" "value") nil))
          (body-cycle (cons 1 nil)))
      (setf (cdr header-cycle) header-cycle
            (cdr body-cycle) body-cycle)
      (signals http-invalid-header
        (make-http-request :method "GET"
                           :uri "https://example.test/"
                           :headers header-cycle))
      (signals http-protocol-error
        (make-http-request :method "GET"
                           :uri "https://example.test/"
                           :body body-cycle))))
  (it "returns no reason or class for unknown status values"
    (expect (http-status-reason 99) :to-be nil)
    (dolist (predicate (list #'http-information-status-p
                             #'http-success-status-p
                             #'http-redirection-status-p
                             #'http-client-error-status-p
                             #'http-server-error-status-p))
      (expect (funcall predicate nil) :to-be nil))))
