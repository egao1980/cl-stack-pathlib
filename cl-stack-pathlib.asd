(defsystem "cl-stack-pathlib"
  :version "0.6.0"
  :description "CLOS paths + pluggable filesystems (pathlib/NIO-style) for cl-stack"
  :author "egao1980"
  :license "MIT"
  :depends-on ("uiop" (:feature :sbcl "sb-posix")
               "compression-protocol" "compression-backend-chipz")

  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "utf8")
               (:file "protocol")
               (:file "path")
               (:file "hash")
               (:file "ignore")
               (:file "walk")
               (:file "local")
               (:file "memory")
               (:file "facade")
               (:file "zip")
               (:file "extensions")
               (:file "snapshot")
               (:file "area"))
  :in-order-to ((test-op (test-op "cl-stack-pathlib/tests"))))

(defsystem "cl-stack-pathlib/tests"
  :depends-on ("cl-stack-pathlib" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "pure-path-test")
               (:file "boost-path-test")
               (:file "normalize-test")
               (:file "fs-ops-test")
               (:file "restarts-test")
               (:file "uri-zip-test")
               (:module "fixtures"
                :components ((:static-file "gitignore-corpus.sexp")
                             (:static-file "gitignore-cases.txt")
                             (:static-file "gen-gitignore-corpus.py")))
               (:file "h2a-support")
               (:file "hash-test")
               (:file "read-range-test")
               (:file "atomic-write-test")
               (:file "ignore-test")
               (:file "walk-ignore-test")
               (:file "grep-test")
               (:file "stat-test")
               (:file "r4a-support")
               (:file "snapshot-test")
               (:file "area-test")
               (:file "utf8-test")
               (:file "glob-test")
               (:file "symlink-walk-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
