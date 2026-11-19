(in-package #:cl-stack-pathlib)

;;; H2a filesystem extensions: portable default methods on FILESYSTEM, their
;;; helpers, and the facade. The value classes FILE-STAT and GREP-MATCH live
;;; in protocol.lisp. The defaults work for every backend that implements the
;;; base protocol (memory and zip get them as-is); local specializes where
;;; streaming or the OS matters.

;;; --- values ----------------------------------------------------------------

(defun file-stat-hash (stat)
  "Content digest (OCTETS-HASH format) of a :FILE stat, computed through
FS-CONTENT-HASH on the first call (not by FS-STAT) and cached; NIL for other
kinds."
  (check-type stat file-stat)
  (when (eq (file-stat-kind stat) :file)
    (or (%file-stat-hash-cache stat)
        (setf (%file-stat-hash-cache stat)
              (fs-content-hash (%file-stat-filesystem stat) (file-stat-path stat))))))

(defmethod print-object ((m grep-match) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~A:~D:~D"
            (let ((p (grep-match-path m)))
              (if (path-p p) (as-posix p) (ignore-errors (uiop:unix-namestring p))))
            (grep-match-line m) (grep-match-column m))))

(defmethod print-object ((st file-stat) stream)
  (print-unreadable-object (st stream :type t)
    (format stream "~S ~A ~D" (file-stat-kind st)
            (ignore-errors (%pathname-as-posix (file-stat-path st)))
            (file-stat-size st))))

;;; --- helpers ---------------------------------------------------------------

(defun %check-range (start end)
  (check-type start (integer 0))
  (check-type end (or null (integer 0)))
  (when (and end (> start end))
    (error "FS-READ-RANGE: START ~D is after END ~D" start end)))

(defun %split-lines (text)
  "Lines of TEXT: split at LF, one trailing CR removed from each line; a
final LF does not start an extra empty line."
  (let ((out '()) (n (length text)) (i 0))
    (loop while (< i n)
          do (let* ((j (or (position #\Newline text :start i) n))
                    (e (if (and (> j i) (char= (char text (1- j)) #\Return)) (1- j) j)))
               (push (subseq text i e) out)
               (setf i (1+ j))))
    (nreverse out)))

(defun %select-lines (lines start end)
  "(values lines-START..END more-p) of the list LINES (1-based, inclusive)."
  (let* ((n (length lines))
         (s (1- start)))
    (if (>= s n)
        (values nil nil)
        (let ((e (if end (min end n) n)))
          (values (subseq lines s (max s e))
                  (and end (> n end) t))))))

(defun %check-open-args (fs pathname direction element-type)
  "→ :OCTETS or :CHARACTERS for FS-OPEN, or signal UNSUPPORTED-OPERATION."
  (cond ((not (eq direction :input))
         (%unsupported fs (format nil "FS-OPEN :DIRECTION ~S" direction) pathname))
        ((subtypep element-type 'character) :characters)
        ((and (subtypep element-type '(unsigned-byte 8))
              (subtypep '(unsigned-byte 8) element-type))
         :octets)
        (t (%unsupported fs (format nil "FS-OPEN :ELEMENT-TYPE ~S" element-type) pathname))))

(defun %utf8-decode-strict (bytes)
  "String from the octets BYTES, or NIL when they are not well-formed UTF-8
(RFC 3629: no overlongs, surrogates, code points past U+10FFFF, stray or
missing continuation octets)."
  (let* ((n (length bytes))
         (out (make-string n))
         (i 0)
         (j 0))
    (loop
      (when (>= i n) (return (subseq out 0 j)))
      (let ((b0 (aref bytes i)) len cp min)
        (cond ((< b0 #x80) (setf len 1 cp b0 min 0))
              ((<= #xC2 b0 #xDF) (setf len 2 cp (logand b0 #x1F) min #x80))
              ((<= #xE0 b0 #xEF) (setf len 3 cp (logand b0 #x0F) min #x800))
              ((<= #xF0 b0 #xF4) (setf len 4 cp (logand b0 #x07) min #x10000))
              (t (return nil)))
        (when (> (+ i len) n) (return nil))
        (loop for k from 1 below len
              for b = (aref bytes (+ i k))
              do (if (= (logand b #xC0) #x80)
                     (setf cp (logior (ash cp 6) (logand b #x3F)))
                     (return-from %utf8-decode-strict nil)))
        (when (or (< cp min) (<= #xD800 cp #xDFFF) (> cp #x10FFFF))
          (return nil))
        (let ((ch (code-char cp)))
          (unless ch (return nil))
          (setf (char out j) ch))
        (incf i len)
        (incf j)))))

;;; Octet input stream over a vector, on the implementation's own Gray
;;; streams (FS-OPEN for memory, zip and any backend without a native stream).

#+(or sbcl ecl)
(progn
  (defclass %octet-input-stream (#+sbcl sb-gray:fundamental-binary-input-stream
                                 #+ecl gray:fundamental-binary-input-stream)
    ((octets :initarg :octets :reader %octet-input-stream-octets)
     (pos :initform 0 :accessor %octet-input-stream-pos))
    (:documentation "Binary input stream reading the octet vector OCTETS."))

  (defmethod #+sbcl sb-gray:stream-read-byte #+ecl gray:stream-read-byte
      ((s %octet-input-stream))
    (let ((octets (%octet-input-stream-octets s))
          (pos (%octet-input-stream-pos s)))
      (if (< pos (length octets))
          (prog1 (aref octets pos)
            (setf (%octet-input-stream-pos s) (1+ pos)))
          :eof)))

  (defmethod #+sbcl sb-gray:stream-read-sequence #+ecl gray:stream-read-sequence
      ((s %octet-input-stream) sequence &optional (start 0) end)
    (let* ((octets (%octet-input-stream-octets s))
           (pos (%octet-input-stream-pos s))
           (end (or end (length sequence)))
           (n (max 0 (min (- end start) (- (length octets) pos)))))
      (replace sequence octets :start1 start :end1 (+ start n) :start2 pos)
      (setf (%octet-input-stream-pos s) (+ pos n))
      (+ start n)))

  (defmethod #+sbcl stream-element-type #+ecl gray:stream-element-type
      ((s %octet-input-stream))
    '(unsigned-byte 8)))

(defun %make-octet-input-stream (fs pathname octets)
  #+(or sbcl ecl)
  (progn fs pathname
         (make-instance '%octet-input-stream :octets octets))
  #-(or sbcl ecl)
  (progn octets
         (%unsupported fs "FS-OPEN of octets (needs Gray streams)" pathname)))

(defun %octets-input-stream (fs pathname octets kind)
  "Input stream of KIND (:OCTETS or :CHARACTERS, UTF-8) over OCTETS."
  (ecase kind
    (:characters (make-string-input-stream (utf8-octets-to-string octets)))
    (:octets (%make-octet-input-stream fs pathname octets))))

;;; --- portable defaults -----------------------------------------------------

(defmethod fs-read-range ((fs filesystem) pathname start end)
  (%check-range start end)
  (let* ((bytes (fs-read-bytes fs pathname))
         (n (length bytes))
         (s (min start n))
         (e (min (or end n) n)))
    (coerce (subseq bytes s (max s e)) '(simple-array (unsigned-byte 8) (*)))))

(defmethod fs-read-lines ((fs filesystem) pathname &key (start 1) end)
  (check-type start (integer 1))
  (check-type end (or null (integer 0)))
  (%select-lines (%split-lines (utf8-octets-to-string (fs-read-bytes fs pathname)))
                 start end))

(defmethod fs-open ((fs filesystem) pathname &key (direction :input) (element-type '(unsigned-byte 8)))
  (let ((kind (%check-open-args fs pathname direction element-type)))
    (%octets-input-stream fs pathname (fs-read-bytes fs pathname) kind)))

(defmethod fs-content-hash ((fs filesystem) pathname)
  (when (fs-directory-p fs pathname)
    (error 'path-error :filesystem fs :path pathname :message "is a directory"))
  (octets-hash (fs-read-bytes fs pathname)))

(defmethod fs-write-atomic ((fs filesystem) pathname bytes &key expected-hash)
  (declare (ignore bytes expected-hash))
  (%unsupported fs 'fs-write-atomic pathname))

(defmethod fs-stat ((fs filesystem) pathname &key (follow-symlinks t))
  (let ((pn (fs-parse fs pathname)))
    (flet ((make (kind size mtime)
             (make-instance 'file-stat :path pn :kind kind :size size :mtime mtime
                                       :mode nil :filesystem fs)))
      (cond ((and (not follow-symlinks) (fs-symlink-p fs pn))
             (make :symlink 0 nil))
            (t
             (let ((real (if (fs-symlink-p fs pn) (fs-resolve fs pn :strict t) pn)))
               (cond ((fs-directory-p fs real) (make :directory 0 nil))
                     ((fs-file-p fs real)
                      (make :file (fs-file-size fs real) (fs-last-modified fs real)))
                     ((fs-exists-p fs real) (make :other 0 nil))
                     (t (error 'path-not-found :filesystem fs :path pn
                                               :message "path does not exist")))))))))

(defmethod fs-grep-matcher ((fs filesystem) pattern &key case-insensitive)
  (declare (ignore pattern case-insensitive))
  (%unsupported fs 'fs-grep-matcher nil))

(defun %grep-line-matcher (fs pattern regex case-insensitive)
  "Function of a line → match start or NIL."
  (cond ((functionp pattern) pattern)
        (regex (fs-grep-matcher fs pattern :case-insensitive case-insensitive))
        (t (check-type pattern string)
           (let ((test (if case-insensitive #'char-equal #'char=)))
             (lambda (line) (search pattern line :test test))))))

(defun %grep-text (bytes)
  "Decoded text of BYTES, or NIL for binary (NUL in the first 8000 octets)
or invalid UTF-8 content."
  (unless (find 0 bytes :end (min 8000 (length bytes)))
    (%utf8-decode-strict bytes)))

(defmethod fs-grep ((fs filesystem) root pattern
                    &key (ignore :auto) include regex case-insensitive max-results)
  (check-type max-results (or null (integer 0)))
  (check-type include (or null string list))
  (let* ((match (%grep-line-matcher fs pattern regex case-insensitive))
         (root (fs-absolute fs root))
         (directory-p (fs-directory-p fs root))
         (include (when include
                    (parse-ignore (if (listp include) include (list include)))))
         (files (if directory-p
                    (let ((all (loop for (nil files nil) in (fs-walk fs root :ignore ignore)
                                     append files)))
                      (if include
                          (let ((prefix (%posix-prefix fs root)))
                            (remove-if-not (lambda (f)
                                             (let ((rel (%relpath prefix f)))
                                               (and rel (ignore-path-p include rel))))
                                           all))
                          all))
                    (when (or (null include)
                              (ignore-path-p include (%entry-name root)))
                      (list root))))
         (files (sort (copy-list files) #'string< :key #'namestring))
         (out '())
         (count 0))
    (dolist (file files)
      (let ((text (%grep-text (fs-read-bytes fs file))))
        (when text
          (loop for line in (%split-lines text)
                for no from 1
                for hit = (funcall match line)
                when hit
                  do (when (and max-results (>= count max-results))
                       (return-from fs-grep (values (nreverse out) t)))
                     (incf count)
                     (push (make-instance 'grep-match :path file :line no
                                                      :column (if (integerp hit) hit 0)
                                                      :text line)
                           out)))))
    (values (nreverse out) nil)))

;;; --- facade ----------------------------------------------------------------

(defun read-range (designator start &optional end)
  "Octets [START, END) of DESIGNATOR (see FS-READ-RANGE)."
  (let ((p (ensure-path designator)))
    (fs-read-range (path-filesystem p) (path-pathname p) start end)))

(defun read-lines (designator &key (start 1) end)
  "Lines START..END of DESIGNATOR, 1-based inclusive (see FS-READ-LINES)."
  (let ((p (ensure-path designator)))
    (fs-read-lines (path-filesystem p) (path-pathname p) :start start :end end)))

(defun open-path (designator &key (direction :input) (element-type '(unsigned-byte 8)))
  (let ((p (ensure-path designator)))
    (fs-open (path-filesystem p) (path-pathname p)
             :direction direction :element-type element-type)))

(defmacro with-open-path ((var designator &rest keys) &body body)
  "Bind VAR to (OPEN-PATH DESIGNATOR . KEYS); close it on unwind."
  `(let ((,var (open-path ,designator ,@keys)))
     (unwind-protect (progn ,@body)
       (close ,var))))

(defun content-hash (designator)
  (let ((p (ensure-path designator)))
    (fs-content-hash (path-filesystem p) (path-pathname p))))

(defun stat (designator &key (follow-symlinks t))
  (let ((p (ensure-path designator)))
    (fs-stat (path-filesystem p) (path-pathname p) :follow-symlinks follow-symlinks)))

(defun grep (root pattern &rest keys &key ignore include regex case-insensitive max-results)
  "FS-GREP over ROOT; GREP-MATCH-PATH values are PATH objects."
  (declare (ignore ignore include regex case-insensitive max-results))
  (let* ((p (ensure-path root))
         (fs (path-filesystem p)))
    (multiple-value-bind (matches truncated-p)
        (apply #'fs-grep fs (path-pathname p) pattern keys)
      (values (mapcar (lambda (m)
                        (make-instance 'grep-match
                                       :path (%wrap fs (grep-match-path m))
                                       :line (grep-match-line m)
                                       :column (grep-match-column m)
                                       :text (grep-match-text m)))
                      matches)
              truncated-p))))
