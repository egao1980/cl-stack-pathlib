(in-package #:cl-stack-pathlib)

;;; Content hashes (H2a). Pure-Lisp SHA-256 (FIPS 180-4) so pathlib keeps no
;;; crypto dependency. Digest strings use the OCI form "sha256:<64 hex>",
;;; lowercase, so they can serve as content-addressed blob ids (R4a) and as
;;; EXPECTED-HASH tokens for compare-and-swap writes.
;;;
;;; The implementation needs an incremental core (init / update over an octet
;;; range / final) so FS-CONTENT-HASH on the local backend can stream a file
;;; in chunks instead of reading it whole.

(deftype %u32 () '(unsigned-byte 32))
(deftype %octet-buffer () '(simple-array (unsigned-byte 8) (*)))

(declaim (type (simple-array (unsigned-byte 32) (64)) +sha256-k+))
(defparameter +sha256-k+
  (make-array 64 :element-type '(unsigned-byte 32)
                 :initial-contents
                 '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
                   #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
                   #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
                   #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
                   #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
                   #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
                   #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
                   #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))
  "SHA-256 round constants (FIPS 180-4, 4.2.2).")

(defstruct (%sha256 (:constructor %sha256-init ()) (:copier nil))
  "Incremental SHA-256 state: chaining value H, a partial block BUF holding
FILL octets, the total octet COUNT and the message schedule scratch W."
  (h (make-array 8 :element-type '(unsigned-byte 32)
                   :initial-contents '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                                       #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
     :type (simple-array (unsigned-byte 32) (8)))
  (buf (make-array 64 :element-type '(unsigned-byte 8))
       :type (simple-array (unsigned-byte 8) (64)))
  (fill 0 :type (integer 0 64))
  (count 0 :type unsigned-byte)
  (w (make-array 64 :element-type '(unsigned-byte 32))
     :type (simple-array (unsigned-byte 32) (64))))

(defmacro %u32-op (form)
  "FORM, a fixnum expression over u32 operands (a sum of two, a shift left
by < 32), declared so: ECL then open-codes it instead of calling generic
arithmetic (the hash was ~3x slower there). Only where fixnums are wide
enough; elsewhere FORM as is."
  (if (>= most-positive-fixnum (expt 2 58)) `(the fixnum ,form) form))

(defun %sha256-block (h w block offset)
  "Compress the 64 octets of BLOCK starting at OFFSET into H."
  (declare (type (simple-array (unsigned-byte 32) (8)) h)
           (type (simple-array (unsigned-byte 32) (64)) w)
           (type %octet-buffer block)
           (type (integer 0 #.(- array-dimension-limit 64)) offset)
           (optimize speed (safety 0)))
  (macrolet ((shr (x n) `(the %u32 (ash (the %u32 ,x) ,(- n))))
             (rotr (x n)
               `(the %u32 (logior (the %u32 (logand #xffffffff (%u32-op (ash (the %u32 ,x) ,(- 32 n)))))
                                  (shr ,x ,n))))
             (xor3 (a b c) `(the %u32 (logxor (the %u32 (logxor ,a ,b)) ,c)))
             (u32+ (&rest xs)
               (reduce (lambda (a b) `(the %u32 (logand #xffffffff (%u32-op (+ (the %u32 ,a) (the %u32 ,b)))))) xs)))
    (dotimes (i 16)
      (let ((j (+ offset (* 4 i))))
        (declare (type fixnum j))
        (setf (aref w i)
              (the %u32 (logior (the %u32 (logior (the %u32 (ash (aref block j) 24))
                                                  (the %u32 (ash (aref block (+ j 1)) 16))))
                                (the %u32 (logior (the %u32 (ash (aref block (+ j 2)) 8))
                                                  (aref block (+ j 3)))))))))
    (loop for i of-type (integer 16 64) from 16 below 64
          do (let* ((w15 (aref w (- i 15)))
                    (w2 (aref w (- i 2)))
                    (s0 (xor3 (rotr w15 7) (rotr w15 18) (shr w15 3)))
                    (s1 (xor3 (rotr w2 17) (rotr w2 19) (shr w2 10))))
               (declare (type %u32 w15 w2 s0 s1))
               (setf (aref w i) (u32+ (aref w (- i 16)) s0 (aref w (- i 7)) s1))))
    (let ((k +sha256-k+)
          (a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
          (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
      (declare (type (simple-array (unsigned-byte 32) (64)) k)
               (type %u32 a b c d e f g hh))
      (dotimes (i 64)
        (let* ((s1 (xor3 (rotr e 6) (rotr e 11) (rotr e 25)))
               (ch (the %u32 (logxor (the %u32 (logand e f))
                                     (the %u32 (logand (the %u32 (logxor e #xffffffff)) g)))))
               (t1 (u32+ hh s1 ch (aref k i) (aref w i)))
               (s0 (xor3 (rotr a 2) (rotr a 13) (rotr a 22)))
               (maj (xor3 (the %u32 (logand a b)) (the %u32 (logand a c)) (the %u32 (logand b c))))
               (t2 (u32+ s0 maj)))
          (declare (type %u32 s1 ch t1 s0 maj t2))
          (setf hh g g f f e e (u32+ d t1) d c c b b a a (u32+ t1 t2))))
      (setf (aref h 0) (u32+ (aref h 0) a)
            (aref h 1) (u32+ (aref h 1) b)
            (aref h 2) (u32+ (aref h 2) c)
            (aref h 3) (u32+ (aref h 3) d)
            (aref h 4) (u32+ (aref h 4) e)
            (aref h 5) (u32+ (aref h 5) f)
            (aref h 6) (u32+ (aref h 6) g)
            (aref h 7) (u32+ (aref h 7) hh))))
  h)

(defun %sha256-update (state octets &key (start 0) end)
  "Feed OCTETS[START, END) into STATE and return STATE. OCTETS may be any
vector of octets; only a simple (UNSIGNED-BYTE 8) vector is read in place."
  (let ((end (or end (length octets))))
    (check-type start (integer 0))
    (assert (<= start end (length octets)) (start end)
            "Octet range [~D, ~D) is not within a vector of length ~D"
            start end (length octets))
    (unless (typep octets '%octet-buffer)
      (setf octets (coerce (subseq octets start end) '%octet-buffer)
            end (- end start)
            start 0))
    (let ((buf (%sha256-buf state))
          (h (%sha256-h state))
          (w (%sha256-w state))
          (fill (%sha256-fill state))
          (i start))
      (declare (type %octet-buffer octets)
               (type (integer 0 64) fill)
               (type (integer 0) i end))
      (when (plusp fill)
        (let ((n (min (- 64 fill) (- end i))))
          (replace buf octets :start1 fill :start2 i :end2 (+ i n))
          (incf fill n)
          (incf i n)
          (when (= fill 64)
            (%sha256-block h w buf 0)
            (setf fill 0))))
      (when (zerop fill)
        (loop while (<= (+ i 64) end)
              do (%sha256-block h w octets i)
                 (incf i 64))
        (replace buf octets :start1 0 :start2 i :end2 end)
        (setf fill (- end i)))
      (setf (%sha256-fill state) fill)
      (incf (%sha256-count state) (- end start))
      state)))

(defun %sha256-final (state)
  "Pad, finish STATE and return the digest string \"sha256:\" + 64 hex."
  (let* ((bits (ldb (byte 64 0) (* 8 (%sha256-count state))))
         (pad (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0))
         (npad (1+ (mod (- 55 (%sha256-fill state)) 64))))
    (setf (aref pad 0) #x80)
    (%sha256-update state pad :end npad)
    (fill pad 0)
    (dotimes (i 8)
      (setf (aref pad i) (ldb (byte 8 (* 8 (- 7 i))) bits)))
    (%sha256-update state pad :end 8)
    (with-output-to-string (s)
      (write-string "sha256:" s)
      (loop for x across (%sha256-h state)
            do (format s "~(~8,'0x~)" x)))))

(defun octets-hash (octets &key (start 0) end)
  "SHA-256 of OCTETS[START, END) as \"sha256:\" + 64 lowercase hex digits."
  (%sha256-final (%sha256-update (%sha256-init) octets :start start :end end)))

(defun %coerce-octets (bytes)
  "BYTES as a simple (UNSIGNED-BYTE 8) vector; a TYPE-ERROR for any element
that is not an octet (checked before any write starts)."
  (if (typep bytes '%octet-buffer)
      bytes
      (map-into (make-array (length bytes) :element-type '(unsigned-byte 8))
                (lambda (b) (check-type b (unsigned-byte 8)) b)
                bytes)))

;;; --- compare-and-swap ------------------------------------------------------

(defun %check-expected-hash (fs pathname expected-hash current-hash-fn)
  "CAS gate for FS-WRITE-ATOMIC. EXPECTED-HASH NIL: proceed, CURRENT-HASH-FN is
not called. Otherwise compare it with (FUNCALL CURRENT-HASH-FN) (a digest, or
NIL when the file is missing; :ABSENT expects NIL). On mismatch signal
CONTENT-HASH-MISMATCH with restarts OVERWRITE and USE-CURRENT.
Returns (values proceed-p current-hash)."
  (check-type expected-hash (or null string (eql :absent)))
  (if (null expected-hash)
      (values t nil)
      (let ((actual (funcall current-hash-fn)))
        (if (if (eq expected-hash :absent)
                (null actual)
                (equal actual expected-hash))
            (values t actual)
            (restart-case
                (error 'content-hash-mismatch
                       :filesystem fs :path pathname
                       :expected expected-hash :actual actual
                       :message "compare-and-swap write refused")
              (overwrite ()
                :report (lambda (s) (format s "Write ~A anyway" pathname))
                (values t actual))
              (use-current ()
                :report (lambda (s) (format s "Keep the current content of ~A, write nothing"
                                            pathname))
                (values nil actual)))))))
