# Test provenance

Case *ideas* and behavioral expectations for this suite are informed by:

## CPython pathlib

- Upstream tests: `Lib/test/test_pathlib/` (Python 3.12+)
- Upstream license: **PSF License v2** (permissive)
- Upstream: https://github.com/python/cpython

## Boost.Filesystem

- Upstream tests: `libs/filesystem/test/path_test.cpp` (and related)
- Upstream license: **Boost Software License 1.0** ([BSL-1.0](https://www.boost.org/LICENSE_1_0.txt)) — permissive; allows derivative works with copyright notice retention when redistributing Boost source itself
- Upstream: https://github.com/boostorg/filesystem
- Copyright © Beman Dawes, Vladimir Prus, and other Boost contributors

This tree contains **original Common Lisp / Rove tests** — not a copy of
`test_pathlib.py` or Boost `.cpp` sources. No upstream source files are vendored.

Where our API deliberately follows **pathlib** over Boost (documented in
`boost-path-test.lisp`):

| Topic | pathlib / us | Boost.Filesystem |
|-------|--------------|------------------|
| `join` + absolute segment | replaces base | V4 `/=` same; older append differed |
| absolute leading `..` | `/../f` → `/f` | `lexically_normal` keeps `/../f` |
| parent of `/` | `/` | `parent_path("/")` → `""` |

If a concrete input/output vector is taken from either suite with only trivial
adaptation, note it next to that deftest and retain the relevant copyright line:

> Copyright © 2001–present Python Software Foundation; All Rights Reserved.
>
> Copyright © Beman Dawes, Vladimir Prus et al.; Boost Software License 1.0.
