(in-package #:cl-stack-pathlib/tests)

;;; The exported UTF-8 codec (#20, #27): UTF8-ENCODE / UTF8-LENGTH,
;;; UTF8-DECODE (lenient: U+FFFD per maximal invalid subpart) and
;;; UTF8-DECODE-STRICT (UTF8-DECODING-ERROR with USE-VALUE / CONTINUE).

(defun ustr (&rest codes)
  "String of the code points CODES."
  (map 'string #'code-char codes))

(defun ovec (&rest bytes)
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defparameter +fffd+ #xFFFD)

(deftest utf8-encode-and-length
  (let ((s (ustr #x61 #xE9 #x20AC #x1F600)))
    (ok (equalp (utf8-encode s)
                (ovec #x61 #xC3 #xA9 #xE2 #x82 #xAC #xF0 #x9F #x98 #x80)))
    (ok (typep (utf8-encode s) '(simple-array (unsigned-byte 8) (*))))
    (ok (= 10 (utf8-length s)))
    (ok (= 0 (utf8-length "")))
    (ok (equalp (utf8-encode s :start 1 :end 3) (ovec #xC3 #xA9 #xE2 #x82 #xAC)))
    (ok (= 5 (utf8-length s :start 1 :end 3)))
    (ok (string= s (utf8-decode-strict (utf8-encode s))) "round trip")
    (ok (string= s (utf8-decode (utf8-encode s))))))

(deftest utf8-decode-ranges
  (let ((v (ovec #x61 #xC3 #xA9 #x62)))
    (ok (string= (ustr #xE9 #x62) (utf8-decode-strict v :start 1)))
    (ok (string= (ustr #x61) (utf8-decode v :end 1)))
    (ok (typep (utf8-decode v) 'simple-string))
    (ok (string= "abc" (utf8-decode-strict #(97 98 99))) "any octet vector")))

(deftest-parametrize utf8-decode-strict-rejects
    ((octets position)
     :ids ("bad-continuation" "stray-continuation" "f8-lead" "ff" "overlong-2"
           "overlong-3" "surrogate" "past-10ffff" "truncated-3" "truncated-at-end")
     ('(#xC3 #x0A) 0)
     ('(#x61 #x80) 1)
     ('(#xF8 #x80 #x80 #x80) 0)
     ('(#xFF) 0)
     ('(#xC0 #xAF) 0)
     ('(#xE0 #x80 #xAF) 0)
     ('(#xED #xA0 #x80) 0)
     ('(#xF4 #x90 #x80 #x80) 0)
     ('(#x61 #x62 #xE2 #x82) 2)
     ('(#xF0 #x9F #x98) 0))
  (let ((v (apply #'ovec octets)))
    (ok (signals (utf8-decode-strict v) 'utf8-decoding-error))
    (ok (eql position
             (handler-case (progn (utf8-decode-strict v) nil)
               (utf8-decoding-error (c) (utf8-decoding-error-position c)))))
    (ok (equalp v (handler-case (progn (utf8-decode-strict v) nil)
                    (utf8-decoding-error (c) (utf8-decoding-error-octets c)))))))

(deftest-parametrize utf8-decode-lenient
    ((octets codes)
     :ids ("valid" "bad-continuation" "truncated-3" "f0-80-80" "surrogate" "stray" "ff-ff")
     ('(#x61 #xC3 #xA9) '(#x61 #xE9))
     ('(#x61 #xC3 #x0A) '(#x61 #xFFFD #x0A))
     ('(#xE2 #x82 #x61) '(#xFFFD #x61))
     ('(#xF0 #x80 #x80) '(#xFFFD #xFFFD #xFFFD))
     ('(#xED #xA0 #x80) '(#xFFFD #xFFFD #xFFFD))
     ('(#x80 #x61) '(#xFFFD #x61))
     ('(#xFF #xFF) '(#xFFFD #xFFFD)))
  ;; Replacement per maximal subpart (Unicode 15, section 3.9, table 3-8).
  (ok (string= (apply #'ustr codes) (utf8-decode (apply #'ovec octets)))))

(deftest utf8-decode-strict-restarts
  (let ((v (ovec #x61 #xC3 #x0A)))
    (ok (string= "fallback"
                 (handler-bind ((utf8-decoding-error
                                  (lambda (c) (use-value "fallback" c))))
                   (utf8-decode-strict v)))
        "USE-VALUE returns the value for the whole decode")
    (ok (string= (ustr #x61 +fffd+ #x0A)
                 (handler-bind ((utf8-decoding-error #'continue))
                   (utf8-decode-strict v)))
        "CONTINUE decodes leniently")))

(deftest-parametrize read-text-strict
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (root kind)
    (let ((p (join root "bad.txt")))
      (write-bytes p (ovec #x61 #xC3 #x0A))
      (ok (signals (read-text p) 'utf8-decoding-error))
      (ok (string= (ustr #x61 +fffd+ #x0A) (read-text p :errors :replace)))
      (ok (string= (ustr #x61 +fffd+ #x0A)
                   (handler-bind ((utf8-decoding-error #'continue))
                     (read-text p))))
      (write-text p (ustr #xE9))
      (ok (string= (ustr #xE9) (read-text p))))))
