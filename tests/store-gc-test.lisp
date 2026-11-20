(in-package #:cl-stack-pathlib/tests)

;;; 0.8.0 (#39): SNAPSHOT-STORE-IDS, SNAPSHOT-STORE-DELETE and
;;; SNAPSHOT-REACHABLE-IDS — mark and sweep over a snapshot store.

(deftest-parametrize snapshot-store-gc
    ((kind store-kind) :ids ("local/directory" "local/memory" "memory/directory" "memory/memory")
     (:local :directory) (:local :memory) (:memory :directory) (:memory :memory))
  (with-r4a (work store kind store-kind)
    (ok (null (snapshot-store-ids store)) "an empty (or not yet created) store")
    (let ((root (subdir work "tree")))
      (make-tree root '(("a.txt" "a") ("b.txt" "b") ("src/c.lisp" "c")))
      (let* ((s1 (snapshot root :store store))
             (_ (progn (write-text (join root "a.txt") "a2")
                       (unlink (join root "b.txt"))))
             (s2 (snapshot root :store store))
             (all (snapshot-store-ids store)))
        (declare (ignore _))
        (testing "ids"
          (ok (equal all (sort (copy-list all) #'string<)) "sorted")
          (ok (= 6 (length all)) "two manifests + a, b, c, a2")
          (ok (every (lambda (id) (snapshot-store-contains-p store id)) all))
          (ok (member s1 all :test #'string=))
          (ok (member (h "a2") all :test #'string=)))
        (testing "reachable"
          (let ((live (snapshot-reachable-ids store (list s2))))
            (ok (equal live (sort (list s2 (h "a2") (h "c")) #'string<)))
            (ok (equal (snapshot-reachable-ids store (list s1 s2 s1 nil)) all)
                "both snapshots reach everything; duplicates and NIL are fine"))
          (ok (signals (snapshot-reachable-ids store (list (h "never stored"))) 'snapshot-not-found)
              "a missing manifest is an error, not an empty set"))
        (testing "sweep"
          (let ((live (snapshot-reachable-ids store (list s2))))
            (dolist (id (snapshot-store-ids store))
              (unless (member id live :test #'string=)
                (ok (eq t (snapshot-store-delete store id)))))
            (ok (equal live (snapshot-store-ids store)) "only the live set is left")
            (ok (not (snapshot-store-contains-p store s1)))
            (ok (signals (snapshot-store-get store s1) 'snapshot-not-found)))
          (ok (null (snapshot-store-delete store s1)) "deleting again → NIL")
          (ok (null (snapshot-store-delete store "not a digest")))
          (testing "the kept snapshot still restores"
            (write-text (join root "zz.txt") "junk")
            (restore-snapshot root s2 :store store)
            (ok (string= "a2" (read-text (join root "a.txt"))))
            (ok (not (exists-p (join root "zz.txt")))))
          (testing "a deleted blob can be stored again"
            (ok (string= (h "b") (snapshot-store-put store (octets "b"))))
            (ok (member (h "b") (snapshot-store-ids store) :test #'string=))))))))

(deftest directory-store-ids-skip-foreign-files
  (with-backend (work :memory)
    (let* ((store (make-directory-snapshot-store (subdir work "store")))
           (id (snapshot-store-put store (octets "x")))
           (dir (join (directory-snapshot-store-root store) "blobs" "sha256" (subseq id 7 9))))
      (write-text (join dir ".tmpfile.0000abcd.tmp") "partial")
      (write-text (join (directory-snapshot-store-root store) "blobs" "sha256" "README") "x")
      (write-text (join (directory-snapshot-store-root store) "blobs" "sha256" "zz" "abc") "x")
      (ok (equal (list id) (snapshot-store-ids store))))))
