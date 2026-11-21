(in-package #:cl-stack-pathlib/tests)

;;; R4b-0 fake journal mechanisms, registered only inside WITH-FAKE-MECHANISMS
;;; (a LET binding of *WATCH-MECHANISMS*), so no other test sees them.
;;;
;;; The scan-based fakes find what changed by snapshotting the journal root
;;; on each drain (ignored paths included: the drain :AROUND filters them) and
;;; report it in different shapes:
;;;   :FAKE-EXACT     every changed entry path, COMPLETE-P T
;;;   :FAKE-COARSE    the parent directory of each changed entry (the entry
;;;                   itself at the top level), COMPLETE-P T, like a mechanism
;;;                   that coalesces events per directory
;;;   :FAKE-OVERFLOW  as exact up to +FAKE-QUEUE-LIMIT+ paths; beyond that the
;;;                   queue "overflows": COMPLETE-P NIL
;;;   :FAKE-LIAR      as exact minus the greatest path, still COMPLETE-P T:
;;;                   the journal suite must catch it
;;; :FAKE-SCRIPTED answers each drain from a queue the test fills, and
;;; records what the protocol hands it.

(defparameter +fake-queue-limit+ 100)

(defclass scan-journal (journal)
  ((store :initform (make-memory-snapshot-store) :reader scan-journal-store)
   (last :initform nil :accessor scan-journal-last)))

(defmethod initialize-instance :after ((j scan-journal) &key)
  (setf (scan-journal-last j) (snapshot (journal-root j) :store (scan-journal-store j))))

(defun scan-touched (j)
  "Entry paths changed since the previous call (or since the journal was made)."
  (let* ((store (scan-journal-store j))
         (now (snapshot (journal-root j) :store store))
         (changes (snapshot-changes (scan-journal-last j) now :store store)))
    (setf (scan-journal-last j) now)
    (mapcar #'fs-change-path changes)))

(defclass exact-journal (scan-journal) ())
(defclass coarse-journal (scan-journal) ())
(defclass overflow-journal (scan-journal) ())
(defclass liar-journal (scan-journal) ())

(defmethod journal-drain ((j exact-journal) &key settle)
  (declare (ignore settle))
  (values (scan-touched j) t))

(defun parent-or-self (rel)
  (let ((k (position #\/ rel :from-end t)))
    (if k (subseq rel 0 k) rel)))

(defmethod journal-drain ((j coarse-journal) &key settle)
  (declare (ignore settle))
  (values (mapcar #'parent-or-self (scan-touched j)) t))

(defmethod journal-drain ((j overflow-journal) &key settle)
  (declare (ignore settle))
  (let ((paths (scan-touched j)))
    (if (> (length paths) +fake-queue-limit+)
        (values (subseq paths 0 +fake-queue-limit+) nil)
        (values paths t))))

(defmethod journal-drain ((j liar-journal) &key settle)
  (declare (ignore settle))
  (values (butlast (sort (scan-touched j) #'string<)) t))

(defclass scripted-journal (journal)
  ((queue :initform '() :accessor scripted-queue
          :documentation "(paths complete-p) answers, oldest first; empty → (NIL T).")
   (drains :initform 0 :accessor scripted-drains)
   (settles :initform '() :accessor scripted-settles :documentation "SETTLE values seen, newest first.")
   (closes :initform 0 :accessor scripted-closes)
   (root-at-close :initform :unset :accessor scripted-root-at-close
                  :documentation "Whether the root directory existed when the primary close ran.")))

(defun script (journal paths complete-p)
  "Queue the answer of a later drain."
  (setf (scripted-queue journal) (append (scripted-queue journal) (list (list paths complete-p))))
  journal)

(defmethod journal-drain ((j scripted-journal) &key settle)
  (incf (scripted-drains j))
  (push settle (scripted-settles j))
  (let ((answer (pop (scripted-queue j))))
    (if answer
        (values (copy-list (first answer)) (second answer))
        (values nil t))))

(defmethod journal-close ((j scripted-journal))
  (incf (scripted-closes j))
  (setf (scripted-root-at-close j) (and (directory-p (journal-root j)) t))
  nil)

(defparameter +fake-classes+
  '((:fake-exact . exact-journal)
    (:fake-coarse . coarse-journal)
    (:fake-overflow . overflow-journal)
    (:fake-liar . liar-journal)
    (:fake-scripted . scripted-journal)))

(defun register-fake (name &key (rank 10) available-p)
  (let ((class (or (cdr (assoc name +fake-classes+)) (error "no fake ~S" name))))
    (register-watch-mechanism
     name
     (lambda (fs directory matcher)
       (make-instance class :filesystem fs :root directory :ignore matcher :mechanism name))
     :rank rank :available-p available-p)))

(defmacro with-fake-mechanisms ((&rest names) &body body)
  "BODY with *WATCH-MECHANISMS* rebound and the fakes NAMES registered (rank 10)."
  `(let ((*watch-mechanisms* *watch-mechanisms*))
     (dolist (n ',names) (register-fake n))
     ,@body))

(defun law-failures (failures)
  (remove-if-not (lambda (f) (eq :law (getf f :kind))) failures))
