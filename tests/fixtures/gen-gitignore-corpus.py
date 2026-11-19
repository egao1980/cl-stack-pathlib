"""Record git's verdicts for tests/fixtures/gitignore-cases.txt.

Usage (from the repo root):
  uv run python -I tests/fixtures/gen-gitignore-corpus.py \
      tests/fixtures/gitignore-cases.txt > tests/fixtures/gitignore-corpus.sexp

Case format: "=case NAME", then one or more ">DIR" lines (DIR empty for the
root, else "sub") each followed by that .gitignore's lines, then "=paths" and
one path per line (a trailing / makes a directory). Each case runs in a fresh
git repo with core.ignorecase=false and core.excludesFile=/dev/null.
"""
import os, subprocess, sys, tempfile, shutil
src = open(sys.argv[1]).read().split("\n")
cases = []; cur = None; mode = None; ign = None
for line in src:
    if line.startswith("=case "):
        cur = {"name": line[6:], "files": [], "paths": []}; cases.append(cur); mode = None
    elif line == "=paths": mode = "paths"
    elif line.startswith(">") and mode != "paths":
        ign = [line[1:], []]; cur["files"].append(ign); mode = "ign"
    elif mode == "ign": ign[1].append(line)
    elif mode == "paths" and line != "": cur["paths"].append(line)
def lisp_str(s): return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'
out = [";;; gitignore corpus. Expected verdicts recorded from real git (" +
       subprocess.run(["git","--version"],capture_output=True,text=True).stdout.strip() +
       ") with core.ignorecase=false:\n;;; `git check-ignore -q PATH` per path, cross-checked against\n;;; `git ls-files --others --ignored --exclude-standard` for files.\n;;; Regenerate: tests/fixtures/gen-gitignore-corpus.py (see PROVENANCE.md).\n;;; Each case: (:name N :ignore-files ((DIR TEXT) ...) :paths ((PATH IGNORED-P) ...)).\n;;; DIR is \"\" for the root or \"sub/\"; PATH ending in / is a directory."]
out.append("(")
for c in cases:
    d = tempfile.mkdtemp()
    subprocess.run(["git","init","-q",d],check=True)
    subprocess.run(["git","-C",d,"config","core.ignorecase","false"],check=True)
    subprocess.run(["git","-C",d,"config","core.excludesFile","/dev/null"],check=True)
    for p in c["paths"]:
        full = os.path.join(d, p)
        if p.endswith("/"): os.makedirs(full, exist_ok=True)
        else:
            os.makedirs(os.path.dirname(full), exist_ok=True); open(full,"w").write("x\n")
    for dirp, lines in c["files"]:
        dd = os.path.join(d, dirp); os.makedirs(dd, exist_ok=True)
        open(os.path.join(dd, ".gitignore"), "w").write("\n".join(lines) + ("\n" if lines else ""))
    ls = set(subprocess.run(["git","-C",d,"ls-files","--others","--ignored","--exclude-standard","-z"],capture_output=True,text=True).stdout.split("\0"))
    rows = []
    for p in c["paths"]:
        r = subprocess.run(["git","-C",d,"check-ignore","-q","--",p.rstrip("/") if p.endswith("/") else p])
        ig = (r.returncode == 0)
        if not p.endswith("/") and (p in ls) != ig:
            print("DISAGREE", c["name"], p, "check-ignore", ig, "ls-files", p in ls, file=sys.stderr)
        rows.append((p, ig))
    shutil.rmtree(d)
    files = " ".join("(%s %s)" % (lisp_str(dp + ("/" if dp else "")), lisp_str("\n".join(ls_) + "\n")) for dp, ls_ in c["files"])
    out.append(" (:name %s\n  :ignore-files (%s)\n  :paths (%s))" % (lisp_str(c["name"]), files,
               "\n          ".join("(%s %s)" % (lisp_str(p), "t" if ig else "nil") for p, ig in rows)))
out.append(")")
print("\n".join(out))
