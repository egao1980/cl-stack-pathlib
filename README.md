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

