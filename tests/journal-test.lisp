(in-package #:cl-stack-pathlib/tests)

;;; R4b-0: change journals. The law (journal = full rescan) through the
;;; journal suite with :POLL and the fakes, the suite catching a liar, the
;;; drain :AROUND (normalization, settle, closed journals), the registry,
;;; FS-WATCH, and areas with journals (attach, narrowing, close).

;;; --- the law -------------------------------------------------------------------

(deftest-parametrize journal-law-poll
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (multiple-value-bind (failures steps) (run-journal-suite :poll :backend kind)
    (ok (null failures) (format nil "no failures: ~S" failures))
    (ok (equal (journal-suite-steps :backend kind) steps) "every applicable step ran")
    (ok (member :burst steps))
    (ok (member :case-only-rename steps))
    (ok (eq (eq kind :local) (and (member :child-process steps) t))
        "child-process edits run on the local backend")))

(deftest-parametrize journal-law-fakes
    ((mechanism kind)
     :ids ("exact/local" "exact/memory" "coarse/local" "coarse/memory"
           "overflow/local" "overflow/memory")
     (:fake-exact :local) (:fake-exact :memory)
     (:fake-coarse :local) (:fake-coarse :memory)
     (:fake-overflow :local) (:fake-overflow :memory))
  (with-fake-mechanisms (:fake-exact :fake-coarse :fake-overflow)
    (multiple-value-bind (failures steps) (run-journal-suite mechanism :backend kind)
      (ok (null failures) (format nil "~S keeps the law: ~S" mechanism failures))
      (ok (equal (journal-suite-steps :backend kind) steps)))))

(deftest-parametrize journal-law-auto
    ((kind) :ids ("local" "memory") (:local) (:memory))
  ;; :AUTO with the exact fake registered: the area gets it and keeps the law.
  (with-fake-mechanisms (:fake-exact)
    (ok (null (run-journal-suite :auto :backend kind)))
    (ok (null (run-journal-suite t :backend kind)))))

(deftest-parametrize journal-suite-catches-liar
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-fake-mechanisms (:fake-liar)
    (let* ((failures (run-journal-suite :fake-liar :backend kind))
           (law (law-failures failures)))
      (ok law "an under-reporting mechanism breaks the law")
      (ok (find :modify-same-size law :key (lambda (f) (getf f :step)))
          "the one-path step catches the dropped path")
      (let ((f (find :modify-same-size law :key (lambda (f) (getf f :step)))))
        (ok (and f (find "a.txt" (getf f :rescan) :key #'second :test #'string=)))
        (ok (and f (not (find "a.txt" (getf f :journal) :key #'second :test #'string=))))))))

(deftest-parametrize journal-suite-reports-open-errors
    ((kind) :ids ("local" "memory") (:local) (:memory))
  ;; A mechanism the area cannot get is a failure of the run, not a pass.
  (with-fake-mechanisms (:fake-exact)
    (let ((failures (run-journal-suite :no-such-mechanism :backend kind)))
      (ok (= 1 (length failures)))
      (ok (eq :open (getf (first failures) :step)))
      (ok (eq :error (getf (first failures) :kind)) "unsupported-operation at open"))))

;;; --- fs-watch and the :poll journal ------------------------------------------------

(deftest-parametrize fs-watch-poll
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (work kind)
    (let* ((dir (subdir work "tree"))
           (fs (path-filesystem work)))
      (area-tree dir)
      (let ((j (fs-watch fs (path-pathname dir))))
        (ok (journal-p j))
        (ok (eq :poll (journal-mechanism j)) "no mechanism registered: :poll")
        (ok (journal-open-p j))
        (ok (eq fs (journal-filesystem j)))
        (ok (same-directory-p dir (journal-root j)))
        (ok (null (journal-ignore j)))
        (external-write (join dir "a.txt") "edited")
        (ok (equal '(nil nil) (multiple-value-list (journal-drain j))) "poll: rescan everything")
        (ok (equal '(nil nil) (multiple-value-list (journal-drain j :settle 0))))
        (journal-close j)
        (ok (not (journal-open-p j)))
        (ok (null (journal-close j)) "close is idempotent")
        (ok (signals (journal-drain j) 'journal-error) "drain after close"))
      ;; :IGNORE :AUTO is resolved once, against DIRECTORY.
      (let ((j (fs-watch fs (path-pathname dir) :ignore :auto)))
        (ok (ignore-matcher-p (journal-ignore j)))
        (ok (ignore-path-p (journal-ignore j) "x.log"))
        (journal-close j))
      ;; :POLL explicitly, even when a better mechanism is registered.
      (with-fake-mechanisms (:fake-exact)
        (let ((j (fs-watch fs (path-pathname dir) :mechanism :poll)))
          (ok (eq :poll (journal-mechanism j)))
          (journal-close j))
        (let ((j (fs-watch fs (path-pathname dir))))
          (ok (eq :fake-exact (journal-mechanism j)) ":auto takes the registered mechanism")
          (journal-close j))))))

(deftest-parametrize fs-watch-errors
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (work kind)
    (let ((fs (path-filesystem work))
          (dir (subdir work "tree")))
      (area-tree dir)
      (ok (signals (fs-watch fs (path-pathname (subdir work "missing"))) 'path-not-found))
      (ok (signals (fs-watch fs (path-pathname dir) :mechanism :no-such-mechanism)
              'unsupported-operation))
      (let ((*watch-mechanisms* *watch-mechanisms*))
        (register-fake :fake-exact :available-p (lambda (fs d) (declare (ignore fs d)) nil))
        (ok (signals (fs-watch fs (path-pathname dir) :mechanism :fake-exact) 'unsupported-operation)
            "registered but unavailable")
        (let ((j (fs-watch fs (path-pathname dir))))
          (ok (eq :poll (journal-mechanism j)) ":auto skips an unavailable mechanism")
          (journal-close j))))))

;;; --- the registry --------------------------------------------------------------------

(deftest watch-mechanism-registry
  (let ((outer *watch-mechanisms*)
        (outer-names (watch-mechanisms)))
    (ok (eq :poll (car (last outer-names))) ":poll is always last")
    (let ((*watch-mechanisms* *watch-mechanisms*))
      (ok (eq :fake-exact (register-fake :fake-exact :rank 1)))
      (register-fake :fake-coarse :rank 5)
      (register-fake :fake-overflow :rank 1)
      (let ((names (watch-mechanisms)))
        (ok (equal '(:fake-coarse :fake-exact :fake-overflow)
                   (subseq names 0 3))
            "highest rank first; equal ranks in registration order")
        (ok (eq :poll (car (last names)))))
      ;; Replacing keeps the place; a new rank reorders.
      (register-fake :fake-exact :rank 1)
      (ok (equal '(:fake-coarse :fake-exact :fake-overflow) (subseq (watch-mechanisms) 0 3)))
      (register-fake :fake-overflow :rank 9)
      (ok (equal '(:fake-overflow :fake-coarse :fake-exact) (subseq (watch-mechanisms) 0 3)))
      (ok (= 1 (count :fake-exact (watch-mechanisms))) "no duplicate names")
      ;; Availability per filesystem and directory.
      (register-fake :fake-scripted :rank 20
                                    :available-p (lambda (fs d)
                                                   (declare (ignore d))
                                                   (typep fs 'memory-filesystem)))
      (with-backend (work :memory)
        (ok (eq :fake-scripted (first (watch-mechanisms (path-filesystem work) (path-pathname work))))))
      (with-backend (work :local)
        (ok (not (member :fake-scripted (watch-mechanisms (path-filesystem work) (path-pathname work)))))
        (ok (eq :poll (car (last (watch-mechanisms (path-filesystem work) (path-pathname work)))))))
      (ok (member :fake-scripted (watch-mechanisms)) "without FS every registered mechanism is listed")
      ;; Unregister.
      (ok (eq t (unregister-watch-mechanism :fake-coarse)))
      (ok (null (unregister-watch-mechanism :fake-coarse)))
      (ok (not (member :fake-coarse (watch-mechanisms))))
      ;; Reserved names.
      (ok (signals (register-watch-mechanism :poll (lambda (fs d m) (declare (ignore fs d m)) nil)) 'error))
      (ok (signals (register-watch-mechanism :auto (lambda (fs d m) (declare (ignore fs d m)) nil)) 'error)))
    (ok (eq outer *watch-mechanisms*) "a LET binding scopes registrations")
    (ok (equal outer-names (watch-mechanisms)))))

;;; --- drain and close (the :around methods) -----------------------------------------

(defun scripted-watch (work &key ignore)
  "A :FAKE-SCRIPTED journal over WORK/tree/ (the area tree)."
  (let ((dir (subdir work "tree")))
    (area-tree dir)
    (fs-watch (path-filesystem work) (path-pathname dir) :mechanism :fake-scripted :ignore ignore)))

(deftest-parametrize journal-drain-normalizes
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (work kind)
    (with-fake-mechanisms (:fake-scripted)
      (let ((j (scripted-watch work :ignore :auto)))
        (ok (eq :fake-scripted (journal-mechanism j)))
        ;; Deduplicated, sorted, trailing "/" stripped.
        (script j '("src/c.lisp" "b.txt" "a.txt" "b.txt" "src/" "src") t)
        (ok (equal '(("a.txt" "b.txt" "src" "src/c.lisp") t) (multiple-value-list (journal-drain j))))
        ;; Ignored: a file matching *.log, anything below build/.
        (script j '("x.log" "build/out.o" "build/sub/deep.o" "a.txt" "src/y.log") t)
        (ok (equal '(("a.txt") t) (multiple-value-list (journal-drain j))))
        ;; "" (the root itself) and COMPLETE-P NIL: rescan everything.
        (script j '("a.txt" "") t)
        (ok (equal '(nil nil) (multiple-value-list (journal-drain j))))
        (script j '("a.txt" "b.txt") nil)
        (ok (equal '(nil nil) (multiple-value-list (journal-drain j))))
        ;; Nothing queued: the fake answers (NIL T).
        (ok (equal '(nil t) (multiple-value-list (journal-drain j))))
        (journal-close j))
      ;; Without an ignore matcher nothing is dropped.
      (let ((j (fs-watch (path-filesystem work) (path-pathname (subdir work "tree"))
                         :mechanism :fake-scripted)))
        (script j '("x.log" "build/out.o") t)
        (ok (equal '(("build/out.o" "x.log") t) (multiple-value-list (journal-drain j))))
        (journal-close j)))))

(deftest-parametrize journal-drain-settle
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (work kind)
    (with-fake-mechanisms (:fake-scripted)
      (let ((j (scripted-watch work)))
        (ok (= 0.05 *journal-settle*) "default settle: 0.05 s")
        (journal-drain j)
        (ok (eql *journal-settle* (first (scripted-settles j))) "the default reaches the mechanism")
        (journal-drain j :settle 0.5)
        (ok (eql 0.5 (first (scripted-settles j))))
        (let ((*journal-settle* 0))
          (journal-drain j))
        (ok (eql 0 (first (scripted-settles j))) "*journal-settle* is the default")
        (let ((before (scripted-drains j)))
          (ok (signals (journal-drain j :settle -1) 'type-error))
          (ok (signals (journal-drain j :settle :soon) 'type-error))
          (ok (= before (scripted-drains j)) "a bad settle never reaches the mechanism"))
        (journal-close j)))))

(deftest-parametrize journal-close-semantics
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (work kind)
    (with-fake-mechanisms (:fake-scripted)
      (let ((j (scripted-watch work)))
        (ok (journal-open-p j))
        (ok (null (journal-close j)))
        (ok (null (journal-close j)))
        (ok (null (journal-close j)))
        (ok (= 1 (scripted-closes j)) "the mechanism's close runs once")
        (ok (not (journal-open-p j)))
        (let ((drains (scripted-drains j)))
          (ok (signals (journal-drain j) 'journal-error))
          (ok (= drains (scripted-drains j)) "a closed journal never reaches the mechanism"))
        (let ((c (handler-case (progn (journal-drain j) nil)
                   (journal-error (c) c))))
          (ok (and c (eq j (journal-error-journal c)))))))))

;;; --- areas with journals ---------------------------------------------------------------

(deftest-parametrize area-journal-attach
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (work kind)
    ;; :AUTO with nothing registered: no journal (the R4a rescan). T: :POLL.
    (let ((a (fresh-area work 1)))
      (ok (null (area-journal a)))
      (close-area a))
    (let ((a (fresh-area work 2 :journal t)))
      (ok (journal-p (area-journal a)))
      (ok (eq :poll (journal-mechanism (area-journal a))))
      (close-area a))
    (with-fake-mechanisms (:fake-exact)
      (let* ((a (fresh-area work 3))
             (j (area-journal a)))
        (ok (journal-p j) ":auto attaches a registered mechanism")
        (ok (eq :fake-exact (journal-mechanism j)))
        (ok (same-directory-p (area-root a) (journal-root j)) "the journal watches the copy")
        (ok (eq (area-ignore a) (journal-ignore j)) "with the area's matcher")
        (ok (journal-open-p j))
        (close-area a)
        (ok (not (journal-open-p j)) "closed with the area"))
      (let ((a (fresh-area work 4 :journal nil)))
        (ok (null (area-journal a)) "NIL: none, even when one is registered")
        (close-area a))
      (let ((a (fresh-area work 5 :journal :poll)))
        (ok (eq :poll (journal-mechanism (area-journal a))))
        (close-area a))
      (let ((dir (subdir work "area-6"))
            (base (subdir work "base-6")))
        (area-tree base)
        (ok (signals (open-area base :journal :no-such-mechanism :directory dir) 'unsupported-operation))
        (ok (not (exists-p dir)) "nothing is created")))))

(deftest-parametrize area-journal-closed-with-the-area
    ((kind) :ids ("local" "memory") (:local) (:memory))
  (with-backend (work kind)
    (with-fake-mechanisms (:fake-scripted)
      (flet ((check (tag action)
               (let* ((a (fresh-area work tag))
                      (j (area-journal a)))
                 (external-write (join (area-root a) "n.txt") "n")
                 (close-area a :action action)
                 (ok (not (journal-open-p j)) (format nil "~S closes the journal" action))
                 (ok (= 1 (scripted-closes j)))
                 (ok (eq t (scripted-root-at-close j)) "closed before the copy is deleted"))))
        (check 1 :discard)
        (check 2 :keep)
        (check 3 :commit))
      ;; A commit writes the full rescan's changes: the scripted journal hid
      ;; n.txt (every drain answers (NIL T)), the base still gets it.
      (ok (string= "n" (text-at (subdir work "base-3") "n.txt"))
          "a commit never trusts the journal"))
    ;; An aborted commit leaves the area and its journal open.
    (with-fake-mechanisms (:fake-exact)
      (multiple-value-bind (a) (conflict-area work 4)
        (let ((j (area-journal a)))
          (handler-bind ((area-conflict #'invoke-abort-commit))
            (close-area a :action :commit))
          (ok (area-open-p a))
          (ok (journal-open-p j))
          (close-area a)
          (ok (not (journal-open-p j))))))))

(deftest-parametrize area-journal-narrows
    ((kind) :ids ("local" "memory") (:local) (:memory))
  ;; The journal is trusted: with COMPLETE-P T only drained paths are
  ;; re-examined, so a scripted journal that hides an edit hides it from
  ;; FS-AREA-CHANGES (and not from :RESCAN T). This is what makes journals
  ;; faster than the rescan; the suite proves real mechanisms never lie.
  (with-backend (work kind)
    (with-fake-mechanisms (:fake-scripted)
      (let* ((a (fresh-area work 1))
             (j (area-journal a))
             (r (area-root a)))
        (ok (eq :fake-scripted (journal-mechanism j)))
        (area-changes a)                ; the first call may do anything
        (external-write (join r "a.txt") "zzz")
        (script j '() t)
        (ok (null (area-changes a)) "a complete empty drain: no change")
        (ok (equal '((:modified "a.txt")) (change-summary (area-changes a :rescan t))))
        (let ((drains (scripted-drains j)))
          (area-changes a :rescan t)
          (ok (= drains (scripted-drains j)) ":rescan t does not drain"))
        (script j '("a.txt") t)
        (ok (equal '((:modified "a.txt")) (change-summary (area-changes a))) "a drained path is re-examined")
        (ok (equal '((:modified "a.txt")) (change-summary (area-changes a)))
            "and stays a change on later calls")
        ;; A drained directory covers everything below it.
        (external-delete (join r "src/c.lisp"))
        (external-write (join r "src/new/n.lisp") "(n)")
        (script j '("src") t)
        (ok (equal '((:modified "a.txt") (:removed "src/c.lisp") (:added "src/new/n.lisp"))
                   (change-summary (area-changes a))))
        ;; Existence is by exact name in the parent's listing.
        (external-delete (join r "b.txt"))
        (script j '("b.txt") t)
        (ok (equal '((:modified "a.txt") (:removed "b.txt") (:removed "src/c.lisp") (:added "src/new/n.lisp"))
                   (change-summary (area-changes a))))
        ;; Ignored paths are never changes, drained or not.
        (external-write (join r "y.log") "log")
        (script j '("y.log") t)
        (ok (equal 4 (length (area-changes a))))
        ;; COMPLETE-P NIL: the full rescan.
        (external-write (join r "hidden.txt") "h")
        (script j '() nil)
        (ok (find "hidden.txt" (change-summary (area-changes a)) :key #'second :test #'string=))
        ;; The area's drain uses *JOURNAL-SETTLE*.
        (let ((*journal-settle* 0.25))
          (area-changes a))
        (ok (eql 0.25 (first (scripted-settles j))))
        (close-area a)))))
