(in-package #:cl-stack-pathlib)

;;; R4b-0: change journals, the protocol and the portable fallback.
;;;
;;; A journal watches a directory tree and tells, on each drain, which
;;; relative paths were touched since the previous drain, or that it lost
;;; track (COMPLETE-P NIL: the caller rescans everything). FS-WATCH picks a
;;; mechanism from *WATCH-MECHANISMS*, which the fs-backend-* repos fill
;;; (FSEvents, inotify, ReadDirectoryChangesW); with none registered, or
;;; when none is available for the volume, the journal is the :POLL one,
;;; whose every drain is (VALUES NIL NIL). Areas (area.lisp) attach a
;;; journal to their root and narrow FS-AREA-CHANGES to the drained paths.
;;;
;;; Contract stubs: every call of %R4B0-PENDING is replaced by the
;;; implementation, which then deletes %R4B0-PENDING.

(defun %r4b0-pending (what)
  (error "cl-stack-pathlib R4b-0: ~A is not implemented yet" what))

;;; --- conditions ----------------------------------------------------------------

(define-condition journal-error (error)
  ((journal :initarg :journal :reader journal-error-journal :initform nil)
   (message :initarg :message :reader %journal-error-message :initform nil))
  (:documentation "Error about a journal, e.g. a drain after JOURNAL-CLOSE.")
  (:report (lambda (c s)
             (format s "~@[~A ~]journal=~S" (%journal-error-message c) (journal-error-journal c)))))

;;; --- the journal value -----------------------------------------------------------

(defvar *journal-settle* 0.05
  "Default :SETTLE of JOURNAL-DRAIN, in seconds (a non-negative real).")

(defclass journal ()
  ((filesystem :initarg :filesystem :reader journal-filesystem)
   (root :initarg :root :reader %journal-root
         :documentation "Absolute directory pathname on FILESYSTEM.")
   (ignore :initarg :ignore :reader journal-ignore :initform nil
           :documentation "The resolved IGNORE-MATCHER (fixed at FS-WATCH), or NIL.")
   (mechanism :initarg :mechanism :reader journal-mechanism
              :documentation "Keyword naming the mechanism: :POLL, :FSEVENTS, :INOTIFY, :RDCW, ...")
   (state :initarg :state :accessor %journal-state :initform :open
          :documentation ":OPEN or :CLOSED."))
  (:documentation "A change journal over the directory tree under its root.
Made by FS-WATCH. Mechanisms subclass it and specialize the primary methods
of JOURNAL-DRAIN and JOURNAL-CLOSE; the :AROUND methods on JOURNAL check the
state and normalize what a drain returns."))

(defun journal-p (x) (typep x 'journal))

(defun journal-root (journal)
  "PATH of the watched directory."
  (%wrap (journal-filesystem journal) (%journal-root journal)))

(defun journal-open-p (journal)
  (eq (%journal-state journal) :open))

(defmethod print-object ((j journal) stream)
  (print-unreadable-object (j stream :type t)
    (format stream "~S ~A ~S" (ignore-errors (journal-mechanism j))
            (ignore-errors (%pathname-as-posix (%journal-root j)))
            (ignore-errors (%journal-state j)))))

(defclass poll-journal (journal) ()
  (:default-initargs :mechanism :poll)
  (:documentation "The portable journal: it never knows what changed, so
every drain is (VALUES NIL NIL) and the caller rescans."))

;;; --- protocol ------------------------------------------------------------------

(defgeneric fs-watch (fs directory &key ignore mechanism)
  (:documentation "Start a change journal over the tree under the directory
DIRECTORY on FS and return it. IGNORE is NIL, an IGNORE-MATCHER relative to
DIRECTORY or :AUTO (resolved once, here, as for FS-SNAPSHOT). MECHANISM
:AUTO (the default) takes the first of (WATCH-MECHANISMS FS DIRECTORY): the
best-ranked registered mechanism available for DIRECTORY, else :POLL.
:POLL always gives the portable journal; another keyword must name a
registered mechanism available for DIRECTORY, else UNSUPPORTED-OPERATION. A
missing DIRECTORY signals PATH-NOT-FOUND. The journal reports paths touched
after FS-WATCH returns."))

(defgeneric journal-drain (journal &key settle)
  (:documentation "→ (values relative-paths complete-p). RELATIVE-PATHS: the
POSIX path strings, relative to the journal root, touched since the previous
drain (or since FS-WATCH), deduplicated and sorted STRING<. A path P means
that the entry at P, or anything below P when P is (or was) a directory, may
have changed. COMPLETE-P T promises that every entry that changed is at or
below some returned path; COMPLETE-P NIL (overflow, a dropped root, or a
mechanism that cannot tell) means the caller must rescan everything, and
RELATIVE-PATHS is then NIL.
Settle rule: every write that returned before JOURNAL-DRAIN was called is
reported, or COMPLETE-P is NIL. SETTLE (seconds, default *JOURNAL-SETTLE*) is
how long a mechanism may wait for OS events in flight before it gives up and
answers NIL.
The :AROUND method on JOURNAL signals JOURNAL-ERROR on a closed journal,
checks SETTLE, and normalizes the primary method's values: trailing \"/\"
stripped, duplicates removed, sorted; paths below an ignored directory, or
themselves ignored as a file, dropped; \"\" (the root itself) or COMPLETE-P
NIL give (VALUES NIL NIL)."))

(defgeneric journal-close (journal)
  (:documentation "Stop the journal and free what it holds. Idempotent: the
:AROUND method on JOURNAL runs the primary methods only on the first call
and marks the journal closed even when they signal. Returns NIL."))

;;; --- mechanism registry ------------------------------------------------------------

(defvar *watch-mechanisms* '()
  "The registered journal mechanisms, in registration order. Treat it as
opaque: change it with REGISTER-WATCH-MECHANISM and
UNREGISTER-WATCH-MECHANISM, which never modify the old list, so a LET
binding of this variable scopes registrations.")

(defun register-watch-mechanism (name constructor &key (rank 0) available-p)
  "Register (or replace, keeping its place) the journal mechanism NAME, a
keyword other than :POLL and :AUTO (those signal an ERROR). CONSTRUCTOR is
called as (FUNCALL CONSTRUCTOR fs directory matcher) with DIRECTORY an
absolute directory pathname on FS and MATCHER an IGNORE-MATCHER or NIL; it
returns an open JOURNAL whose JOURNAL-MECHANISM is NAME. RANK (an integer)
orders mechanisms, highest first; equal ranks keep registration order.
AVAILABLE-P, NIL (always) or a function of (fs directory), tells whether the
mechanism can watch that directory (the volume, the OS, the filesystem
class). Returns NAME."
  (declare (ignore name constructor rank available-p))
  (%r4b0-pending 'register-watch-mechanism))

(defun unregister-watch-mechanism (name)
  "Remove the mechanism NAME. → T when it was registered, else NIL."
  (declare (ignore name))
  (%r4b0-pending 'unregister-watch-mechanism))

(defun watch-mechanisms (&optional fs directory)
  "Names of the registered mechanisms, best first (rank, then registration
order), followed by :POLL. With FS and DIRECTORY, only the mechanisms
available for that directory; :POLL is always last."
  (declare (ignore fs directory))
  (%r4b0-pending 'watch-mechanisms))

;;; --- default methods -----------------------------------------------------------

(defmethod fs-watch ((fs filesystem) directory &key ignore (mechanism :auto))
  (declare (ignore directory ignore mechanism))
  (%r4b0-pending 'fs-watch))

(defmethod journal-drain :around ((journal journal) &key (settle *journal-settle*))
  (declare (ignore settle))
  (%r4b0-pending 'journal-drain))

(defmethod journal-drain ((journal poll-journal) &key settle)
  (declare (ignore settle))
  (%r4b0-pending 'journal-drain))

(defmethod journal-close :around ((journal journal))
  (%r4b0-pending 'journal-close))

(defmethod journal-close ((journal journal))
  nil)
