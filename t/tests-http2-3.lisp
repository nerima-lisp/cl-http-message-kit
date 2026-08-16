(in-package #:http-message-kit/test)

(describe "HTTP/2 frame codec"
  (it "recognizes and copies the connection preface"
    (let ((preface (http2-connection-preface)))
      (expect (http2-connection-preface-p preface) :to-equalp t)
      (setf (aref preface 0) (1+ (aref preface 0)))
      (expect (http2-connection-preface-p preface) :to-equalp nil)))

  (it "round trips a fixed-size frame"
    (let ((payload (octets 0 1 2 3 4 5 6 7)))
      (expect (http2-frame-type-code :ping) :to-be 6)
      (expect (http2-frame-type-name 6) :to-equalp :ping)
      (let ((wire (http2-encode-frame :ping :payload payload)))
        (expect (length wire) :to-be 17)
        (multiple-value-bind (frame used) (http2-decode-frame wire)
          (expect used :to-be 17)
          (expect (http-frame-protocol frame) :to-equalp :http/2)
          (expect (http-frame-type frame) :to-equalp :ping)
          (expect (http-frame-stream-id frame) :to-be 0)
          (expect (http-frame-payload frame) :to-equalp payload)))))

  (it "preserves flags on unknown extension frames"
    (let ((wire (http2-encode-frame #x2a
                                    :flags #xff
                                    :payload (octets 1 2 3))))
      (multiple-value-bind (frame used) (http2-decode-frame wire)
        (expect used :to-be (length wire))
        (expect (http-frame-type frame) :to-be #x2a)
        (expect (http-frame-flags frame) :to-be #xff)
        (expect (http-frame-payload frame) :to-equalp (octets 1 2 3)))))

  (it "validates padding and priority dependencies"
    (let ((wire (http2-encode-frame :data
                                    :flags #x8
                                    :stream-id 1
                                    :payload (octets 2 65 66 0 0))))
      (multiple-value-bind (frame used) (http2-decode-frame wire)
        (expect used :to-be (length wire))
        (expect (http-frame-payload frame) :to-equalp
                (octets 2 65 66 0 0))))
    (signals http-invalid-frame
      (http2-encode-frame :data :flags #x8 :stream-id 1
                          :payload (octets 5 65)))
    (signals http-invalid-frame
      (http2-encode-frame :headers :flags #x20 :stream-id 1
                          :payload (octets 0 0 0 1 0))))

  (it "rejects reserved stream and flow-control bits"
    (signals http-invalid-frame
      (http2-decode-frame
       (octets 0 0 4 8 0 0 0 0 0 128 0 0 1)))
    (signals http-invalid-frame
      (http2-decode-frame
       (octets 0 0 8 7 0 0 0 0 0 128 0 0 0 0 0 0 0 0))))

  (it "round trips settings and rejects duplicates"
    (let* ((settings '((:enable-push . 0)
                       (:max-frame-size . #x4000)
                       (42 . 7)))
           (encoded (http2-encode-settings settings)))
      (expect (http2-decode-settings encoded) :to-equalp settings)
      (signals http-invalid-frame
        (http2-encode-settings '((:enable-push . 0)
                                 (:enable-push . 1))))))
)

(describe "HTTP/2 and HTTP/3 header sections"
  (it "validates ordinary and CONNECT request pseudo-headers"
    (expect
     (http2-validate-header-section
      (list (make-http-pseudo-header ":method" "GET")
            (make-http-pseudo-header ":scheme" "https")
            (make-http-pseudo-header ":authority" "example.test")
            (make-http-pseudo-header ":path" "/resource")
            (make-http-header "user-agent" "kit"))
      :request-p t)
     :to-equalp
     (list (make-http-pseudo-header ":method" "GET")
           (make-http-pseudo-header ":scheme" "https")
           (make-http-pseudo-header ":authority" "example.test")
           (make-http-pseudo-header ":path" "/resource")
           (make-http-header "user-agent" "kit")))
    (expect
     (http3-validate-header-section
      (list (make-http-pseudo-header ":method" "CONNECT")
            (make-http-pseudo-header ":authority" "example.test"))
      :request-p t)
     :to-equalp
     (list (make-http-pseudo-header ":method" "CONNECT")
           (make-http-pseudo-header ":authority" "example.test"))))

  (it "supports extended CONNECT and response sections"
    (http2-validate-header-section
     (list (make-http-pseudo-header ":method" "CONNECT")
           (make-http-pseudo-header ":protocol" "websocket")
           (make-http-pseudo-header ":scheme" "https")
           (make-http-pseudo-header ":authority" "example.test")
           (make-http-pseudo-header ":path" "/chat"))
     :request-p t)
    (http3-validate-header-section
     (list (make-http-pseudo-header ":status" "204")
           (make-http-header "server" "kit")))
    (signals http-invalid-header
      (http2-validate-header-section
       (list (make-http-pseudo-header ":status" "200"))
       :request-p t))
    (signals http-invalid-header
      (http3-validate-header-section
       (list (make-http-pseudo-header ":method" "GET")))))

  (it "rejects pseudo-header ordering and connection-specific fields"
    (signals http-invalid-header
      (http2-validate-header-section
       (list (make-http-header "x-test" "one")
             (make-http-pseudo-header ":status" "200"))))
    (signals http-invalid-header
      (http2-validate-header-section
       (list (make-http-pseudo-header ":status" "200")
             (make-http-header "connection" "close")))))
)

(describe "HTTP/3 frame and setting codec"
  (it "round trips QUIC variable-length integers at every width"
    (dolist (value '(0 #x3f #x40 #x3fff #x4000 #x3fffffff
                     #x40000000 #x3fffffffffffffff))
      (let ((encoded (http3-encode-varint value)))
        (multiple-value-bind (decoded used) (http3-decode-varint encoded)
          (expect decoded :to-be value)
          (expect used :to-be (length encoded))))))

  (it "round trips an HTTP/3 frame and settings"
    (let* ((settings '((:enable-connect-protocol . 1)
                       (:h3-datagram . 1)))
           (settings-octets (http3-encode-settings settings))
           (wire (http3-encode-frame :settings :payload settings-octets)))
      (expect (http3-decode-settings settings-octets) :to-equalp settings)
      (multiple-value-bind (frame used) (http3-decode-frame wire)
        (expect used :to-be (length wire))
        (expect (http-frame-protocol frame) :to-equalp :http/3)
        (expect (http-frame-type frame) :to-equalp :settings)
        (expect (http-frame-stream-id frame) :to-equalp nil)
        (expect (http-frame-payload frame) :to-equalp settings-octets))))

  (it "accepts the full QUIC varint range for HTTP/3 settings"
    (let* ((settings '((42 . #x3fffffffffffffff)))
           (encoded (http3-encode-settings settings)))
      (expect (http3-decode-settings encoded) :to-equalp settings)))

  (it "rejects invalid HTTP/3 settings and truncated frames"
    (signals http-invalid-frame
      (http3-encode-settings '((:h3-datagram . 2))))
    (signals http-invalid-frame
      (http3-encode-settings '((:enable-connect-protocol . 1)
                               (:enable-connect-protocol . 1))))
    (signals http-incomplete-message
      (http3-decode-frame (octets #x40)))))
