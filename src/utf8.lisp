(in-package #:cl-stack-pathlib)

;;; UTF-8 codec (RFC 3629), pure Lisp so every backend decodes the same way.
;;;
;;; UTF8-DECODE-STRICT signals UTF8-DECODING-ERROR on ill-formed input:
;;; overlongs, surrogates, code points past U+10FFFF, stray or missing
;;; continuation octets, truncated sequences. UTF8-DECODE never signals: each
;;; maximal ill-formed subpart becomes one U+FFFD (Unicode 15, section 3.9,
;;; "U+FFFD Substitution of Maximal Subparts", the WHATWG Encoding choice).

(define-condition utf8-decoding-error (error)
  ((octets :initarg :octets :reader utf8-decoding-error-octets
           :documentation "The octet vector being decoded.")
   (position :initarg :position :reader utf8-decoding-error-position
             :documentation "Index in OCTETS of the first octet of the ill-formed sequence."))
  (:documentation "OCTETS are not well-formed UTF-8. UTF8-DECODE-STRICT (and
READ-TEXT) signal it with the restarts USE-VALUE (return a string instead) and
CONTINUE (decode with U+FFFD replacement characters).")
  (:report (lambda (c s)
             (let* ((v (utf8-decoding-error-octets c))
                    (i (utf8-decoding-error-position c)))
               (format s "invalid UTF-8 at octet ~D~@[ (~{#x~2,'0X~^ ~})~]"
                       i (and v (< i (length v))
                              (coerce (subseq v i (min (length v) (+ i 4))) 'list)))))))

(declaim (inline %utf8-lead-info))
(defun %utf8-lead-info (b0)
  "(values length initial-bits lo2 hi2) for the lead octet B0, or NIL when B0
cannot start a sequence. LO2..HI2 bounds the second octet (RFC 3629, table 3-7),
which rules out overlongs, surrogates and code points past U+10FFFF."
  (cond ((< b0 #x80) (values 1 b0 0 0))
        ((< b0 #xC2) nil)
        ((< b0 #xE0) (values 2 (logand b0 #x1F) #x80 #xBF))
        ((= b0 #xE0) (values 3 (logand b0 #x0F) #xA0 #xBF))
        ((= b0 #xED) (values 3 (logand b0 #x0F) #x80 #x9F))
        ((< b0 #xF0) (values 3 (logand b0 #x0F) #x80 #xBF))
        ((= b0 #xF0) (values 4 (logand b0 #x07) #x90 #xBF))
        ((< b0 #xF4) (values 4 (logand b0 #x07) #x80 #xBF))
        ((= b0 #xF4) (values 4 (logand b0 #x07) #x80 #x8F))
        (t nil)))

(defun %utf8-next (octets i end)
  "Decode one sequence at OCTETS[I] (I < END). Returns (values char length):
CHAR is NIL when the sequence is ill-formed, and LENGTH is then the length of
its maximal subpart (at least 1)."
  (multiple-value-bind (len cp lo hi) (%utf8-lead-info (aref octets i))
    (cond ((null len) (values nil 1))
          ((= len 1) (values (code-char cp) 1))
          (t
           (loop for k from 1 below len
                 for j = (+ i k)
                 do (when (>= j end) (return-from %utf8-next (values nil k)))
                    (let ((b (aref octets j)))
                      (unless (if (= k 1) (<= lo b hi) (<= #x80 b #xBF))
                        (return-from %utf8-next (values nil k)))
                      (setf cp (logior (ash cp 6) (logand b #x3F)))))
           (let ((ch (code-char cp)))
             (if ch (values ch len) (values nil len)))))))

(defun %utf8-decode (octets start end on-error)
  "Decode OCTETS[START, END). ON-ERROR is :REPLACE (U+FFFD), :NIL (return NIL)
or a function of the octet index whose value is returned from the decode."
  (let* ((end (or end (length octets)))
         (out (make-string (- end start)))
         (i start)
         (j 0)
         (fffd (code-char #xFFFD)))
    (loop while (< i end)
          do (multiple-value-bind (ch len) (%utf8-next octets i end)
               (cond (ch (setf (char out j) ch))
                     ((eq on-error :replace) (setf (char out j) fffd))
                     ((eq on-error :nil) (return-from %utf8-decode nil))
                     (t (return-from %utf8-decode (funcall on-error i))))
               (incf i len)
               (incf j)))
    (if (= j (length out)) out (subseq out 0 j))))

(defun %check-octet-range (octets start end)
  (check-type octets vector)
  (check-type start (integer 0))
  (check-type end (or null (integer 0)))
  (unless (<= start (or end (length octets)) (length octets))
    (error "bounding indices ~D, ~S are invalid for a vector of length ~D"
           start end (length octets))))

(defun utf8-decode (octets &key (start 0) end)
  "String from the UTF-8 OCTETS[START, END). Never signals on bad input: each
maximal ill-formed subpart decodes to one U+FFFD REPLACEMENT CHARACTER."
  (%check-octet-range octets start end)
  (%utf8-decode octets start end :replace))

(defun utf8-decode-strict (octets &key (start 0) end)
  "String from the UTF-8 OCTETS[START, END). Ill-formed input signals
UTF8-DECODING-ERROR with the restarts USE-VALUE (return the given string) and
CONTINUE (decode as UTF8-DECODE does)."
  (%check-octet-range octets start end)
  (%utf8-decode octets start end
                (lambda (i)
                  (restart-case (error 'utf8-decoding-error :octets octets :position i)
                    (use-value (value)
                      :report "Return a string supplied instead of the decoded text."
                      :interactive (lambda ()
                                     (format *query-io* "String: ")
                                     (force-output *query-io*)
                                     (list (read-line *query-io*)))
                      value)
                    (continue ()
                      :report "Decode with U+FFFD for each ill-formed sequence."
                      (%utf8-decode octets start end :replace))))))

(defun %utf8-decode-or-nil (octets)
  "Strict decode of OCTETS, or NIL when they are not well-formed UTF-8."
  (%utf8-decode octets 0 nil :nil))

(declaim (inline %utf8-char-length))
(defun %utf8-char-length (code)
  (cond ((< code #x80) 1)
        ((< code #x800) 2)
        ((< code #x10000) 3)
        (t 4)))

(defun %check-string-range (string start end)
  (check-type string string)
  (check-type start (integer 0))
  (check-type end (or null (integer 0)))
  (unless (<= start (or end (length string)) (length string))
    (error "bounding indices ~D, ~S are invalid for a string of length ~D"
           start end (length string))))

(defun utf8-length (string &key (start 0) end)
  "Number of octets UTF8-ENCODE makes from STRING[START, END)."
  (%check-string-range string start end)
  (loop for i from start below (or end (length string))
        sum (%utf8-char-length (char-code (char string i)))))

(defun utf8-encode (string &key (start 0) end)
  "UTF-8 octets of STRING[START, END) as a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
A lone surrogate code point (possible in some implementations' strings) is
encoded as U+FFFD, so the result is always well-formed UTF-8."
  (%check-string-range string start end)
  (let* ((end (or end (length string)))
         (out (make-array (utf8-length string :start start :end end)
                          :element-type '(unsigned-byte 8)))
         (j 0))
    (flet ((put (b) (setf (aref out j) b) (incf j)))
      (loop for i from start below end
            for c = (let ((c (char-code (char string i))))
                      (if (<= #xD800 c #xDFFF) #xFFFD c))
            do (case (%utf8-char-length c)
                 (1 (put c))
                 (2 (put (logior #xC0 (ash c -6)))
                    (put (logior #x80 (logand c #x3F))))
                 (3 (put (logior #xE0 (ash c -12)))
                    (put (logior #x80 (logand (ash c -6) #x3F)))
                    (put (logior #x80 (logand c #x3F))))
                 (t (put (logior #xF0 (ash c -18)))
                    (put (logior #x80 (logand (ash c -12) #x3F)))
                    (put (logior #x80 (logand (ash c -6) #x3F)))
                    (put (logior #x80 (logand c #x3F)))))))
    out))

;;; Internal names used before 0.6.0 (kept for callers that reached in).
(defun utf8-octets-to-string (bytes) (utf8-decode bytes))
(defun utf8-string-to-octets (text) (utf8-encode text))
