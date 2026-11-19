# cl-stack-pathlib

MIT. CLOS **path** values + pluggable **filesystem** protocol for [cl-stack](https://github.com/egao1980/cl-stack).

Python **pathlib** / Java **NIO.2 Path+Files+FileSystem** feature shape, Common Lisp DX:

- pathnames under the hood (UIOP)
- `path` objects bound to a `filesystem`
- keyword-heavy facade (`join`, `absolute` vs `resolve`, `read-text`, …)
- conditions + restarts (`create-parents`, `create-file`, `overwrite`, …)
- escape hatch: `path-pathname`

## Why

OCI install roots and generated `cl-repo-init` must keep path identity (`/tmp` ≠ `/private/tmp`). That is `absolute` (no symlink resolve), not `resolve` (NIO `toRealPath` / pathlib `resolve`).

Virtual FS support: specialize `filesystem` (shipped: `local-filesystem`, `memory-filesystem`, `zip-filesystem`). URI schemes: `file://`, `zip://` (RFC 8089 archive path + `!/` entry: `zip:///tmp/data.zip!/x`, `zip:///C:/app/data.zip!/x`).

## Quick start

```lisp
(asdf:load-system "cl-stack-pathlib")
(use-package :cl-stack-pathlib) ; or (:local-nicknames (#:sp #:cl-stack-pathlib))

(join "/opt/pkg" "native" "libssl.so")
(absolute "relative/x")          ; no symlink follow
(resolve "/etc/passwd")          ; real path

(with-filesystem ((make-memory-filesystem))
  (write-text "/a/b.txt" "hi")
  (read-text "/a/b.txt"))

(with-temp-directory (tmp)
  (write-text (join tmp "x.txt") "hi")
  (copytree tmp "/backup")
  (which "sbcl"))

;; recoveries (CL restarts)
(with-auto-create-parents
  (mkdir "/deep/nested" :parents nil))   ; CREATE-PARENTS + RETRY

(with-auto-create-file
  (read-bytes "/new.txt"))               ; CREATE-FILE (empty) + RETRY

;; zip:// — archive as a filesystem (read-only; stored + deflate)
(write-zip-file "/tmp/data.zip" '(("countries/DE.sexp" "(:code \"DE\")")))
(read-text "zip:///tmp/data.zip!/countries/DE.sexp")
(read-text "zip:///C:/app/data.zip!/countries/DE.sexp") ; Windows — keep the drive
(join (zip-path "/tmp/data.zip") "countries/DE.sexp")
```

Nickname: `stack-pathlib` only — not `path` (clashes with cl-fad / filepaths).

## Agent-tool operations (0.4.0, H2a)

Pure Lisp, no extra dependencies. Portable defaults on `filesystem` (memory and zip get them as-is); `local-filesystem` specializes where streaming or the OS matters.

```lisp
(read-range "big.bin" 1024 2048)              ; octets [START, END)
(read-lines "src/x.lisp" :start 10 :end 20)   ; → (values lines more-p), 1-based
(with-open-path (in "a.txt" :element-type 'character) (read-line in))

(content-hash "a.txt")                        ; "sha256:…" (SHA-256, FIPS 180-4)
(octets-hash #(97 98 99))

;; atomic (temp file + rename(2)) and compare-and-swap writes
(multiple-value-bind (p hash) (write-text "a.txt" "v1" :atomic t)
  (write-text p "v2" :expected-hash hash))    ; CONTENT-HASH-MISMATCH if it changed
(handler-bind ((content-hash-mismatch #'invoke-use-current))   ; or #'auto-overwrite
  (write-text "a.txt" "mine" :expected-hash :absent))

(file-stat-mode (stat "a.txt"))               ; kind / size / mtime / mode; lazy FILE-STAT-HASH

;; .gitignore / .ignore semantics (gitignore(5); verified against git)
(walk "." :ignore :auto)                      ; ignored dirs are pruned, .git/ always
(rglob "." "*.lisp" :ignore (parse-ignore "build/"))
(grep "." "defun" :include "*.lisp" :max-results 50)   ; → (values matches truncated-p)
```

`grep` takes a literal string or a function `line → start | NIL`; `:regex t` goes through the hook `fs-grep-matcher`, which refuses by default (pathlib has no regex engine). Zip is read-only: `fs-write-atomic` signals `unsupported-operation`.

## Snapshots and areas (0.5.0, R4a)

Pure Lisp, portable over the base protocol (local and memory backends).

A **snapshot** is a content-addressed manifest of a tree's regular files and symlinks (directories are not entries; symlinks are not followed). The manifest has a fixed text form: a `cl-stack-pathlib tree v1` header, then one `<mode> <hash> <n> <path>` line per entry, sorted by path. The modes are `100644`, `100755` and `120000`. The snapshot id is the `octets-hash` of that manifest, so the same tree gets the same id on every run, machine and backend. Only the owner execute bit counts toward the mode.

```lisp
(let* ((store (make-directory-snapshot-store "/var/snapshots"))  ; or a memory store
       (id (snapshot "proj/" :ignore :auto :store store)))
  (write-text "proj/README.md" "oops")
  (snapshot-changes id (snapshot "proj/" :ignore :auto :store store) :store store)
  (restore-snapshot "proj/" id :store store :ignore :auto))    ; → FS-CHANGE list applied
```

`restore-snapshot` checks that the manifest and every blob are in the store before it touches anything. A missing one signals `snapshot-not-found`. Ignored paths are never written or deleted, and directories the restore empties are removed. The directory store keeps each blob in `blobs/sha256/<2 hex>/<62 hex>` and writes it atomically.

An **area** is a writable copy of a base directory. Agents can edit it with anything (pathlib, `cl:open`, a shell), and each rescan finds the edits. It uses the racy-git rule: a file whose size or mtime changed, or whose recorded mtime falls within 2 s of the open time, is hashed.

```lisp
(with-area (a "proj/" :ignore :auto)          ; :mechanism :copy (the only portable one)
  (write-text (join (area-root a) "x.lisp") "(new)")
  (area-changes a)                            ; → ((:added "x.lisp" …))
  (handler-bind ((area-conflict #'invoke-skip-path))   ; or OVERWRITE / ABORT-COMMIT
    (close-area a :action :commit)))          ; → (values changes :committed)
```

`close-area` takes `:discard` (the default), `:keep` or `:commit`. A commit checks every changed path first. A path that changed both in the base and in the area, to different results, signals `area-conflict` with the restarts `overwrite`, `skip-path` and `abort-commit`. Nothing is written until every conflict is resolved, and the base paths the area did not change are never touched. Other mechanisms (`:clone`, `:overlay`, …) signal `unsupported-operation`; fs-backend repos specialize the same generics.

## Amalgamates

| Source | Ideas |
|--------|--------|
| Python pathlib | Path value, absolute vs resolve, glob/walk, IO helpers |
| Java NIO.2 | FileSystem SPI, Path bound to FS, Files.* ops |
| fosskers/filepaths | join/name/base/extension clarity |
| ppath | abspath/realpath split, expanduser |
| cl-fad / UIOP | portable probes, copy, temp |

## Protocol (backends)

Specialize generics on `filesystem`: `fs-parse`, `fs-join`, `fs-exists-p`, `fs-read-bytes`, `fs-mkdir`, …  
Default `*filesystem*` is `local-filesystem`.

## Tests

```bash
sbcl --eval '(asdf:load-asd "cl-stack-pathlib.asd")' \
     --eval '(asdf:test-system "cl-stack-pathlib")'
```

## Publish

Source-only OCI publish is centralized in [`cl-stack-systems`](https://github.com/egao1980/cl-stack-systems)
(`imports/cl-stack-pathlib/qlfile` pin + shared `publish.yml`). Packaging metadata lives in the `.asd`
(`auto-package-spec`):

```bash
gh workflow run publish.yml -R egao1980/cl-stack-systems -f import=cl-stack-pathlib
```

