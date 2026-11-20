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

## UTF-8, globs and symlinks (0.6.0)

A pure-Lisp UTF-8 codec (RFC 3629) that behaves the same on every backend and implementation:

```lisp
(utf8-encode "héllo")                 ; → (simple-array (unsigned-byte 8) (*))
(utf8-length "héllo")                 ; → 6 octets
(utf8-decode #(#x61 #xC3 #x0A))       ; lenient: "a�\n" (one U+FFFD per maximal bad subpart)
(utf8-decode-strict octets :start 0 :end n)   ; signals UTF8-DECODING-ERROR
(handler-bind ((utf8-decoding-error #'continue))   ; or (use-value "…")
  (read-text "maybe-latin1.txt"))
(read-text "x.txt" :errors :replace)  ; never signals
```

`utf8-decode-strict` rejects overlongs, surrogates, code points past U+10FFFF, stray or missing continuation octets and truncated sequences. It signals `utf8-decoding-error` (readers `utf8-decoding-error-octets` and `utf8-decoding-error-position`) with the restarts `use-value` and `continue`. `read-text` is strict by default now. `read-lines` and character streams from `open-path` decode leniently.

Glob patterns use fnmatch rules on each `/`-separated segment: `*`, `?`, `[a-z]`, `[!x]` and backslash escapes, none of which matches `/`. A `**` segment matches any number of directories. The same matcher runs on every backend, and names are compared as strings, so `[` in a file name is an ordinary character. `rglob` matches the pattern at any depth, and `match-p` follows `PurePath.match`.

```lisp
(glob "src/" "*.l*")         (glob "." "file?.txt")      (glob "." "[km]*")
(glob "." "**/*.lisp")       (rglob "." "m*.lisp")       (match-p "a/b/c.lisp" "b/*.lisp")
```

Symlinks behave as in `os.walk` / pathlib on every backend. `walk` lists a link to a file (or a dangling link) in FILES and a link to a directory in DIRS. It walks a directory link only with `:follow-symlinks t`, and never one that leads back to a directory being walked. `glob` and `rglob` list links but never descend directory links, and their results stay under the base as spelled. `grep` skips links, as ripgrep does by default. `load-ignore-files` (`:ignore :auto`) never follows a link. `symlink-p` is true for a directory-form pathname of a link (`out/`). Memory filesystems resolve symlinks in parent directories, so `/link-to-dir/file` works there as it does on local. Temp names made by `fs-make-temp` no longer collide within one second.

## Probes follow the final link (0.6.1)

`exists-p`, `file-p`, `directory-p`, `readable-p`, `file-size`, `last-modified` and `stat` follow a symlink at the end of the path, as `stat(2)` does. They answer the same on the local and memory backends (zip archives have no links).

| Path | `exists-p` / `file-p` / `directory-p` | `file-size`, `last-modified`, `stat` |
|------|---------------------------------------|--------------------------------------|
| link to a file | T / T / NIL | the file's |
| directory, or link to one | T / NIL / T | `file-size` 0 (= `stat` `:size`), `stat` is `:directory` |
| dangling link | NIL | `path-not-found` |
| link cycle | NIL | `symlink-loop` (a `path-error`, ELOOP) |

`file-size` of a directory is 0 on the local, memory and zip backends (0.6.2), as its `stat` `:size`; it is never the local `st_size` of the directory and never `path-not-found`.

`symlink-p` and `(stat p :follow-symlinks nil)` stay the `lstat(2)` view. On SBCL, local `file-p` / `exists-p` used to be true for a dangling link, because `probe-file` falls back to `lstat`. They now use `stat(2)` wherever there is a binding (SBCL and ECL on unix). The deprecated internals `%name-matches-p` and `%wild-pattern` are back as shims, so a backend that defines its own `fs-glob` against 0.5 still loads and runs.

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

