"""
Entry point for managing slides.
"""

import sys
import os
import argparse
from pathlib import Path
import subprocess
import toml


CI_DIR = Path(sys.argv[0]).resolve().parent
DEFAULT_COURSE_CONFIG = CI_DIR / "default_course.toml"

ROOT = CI_DIR.parents[1]
COURSES = ROOT / "courses"
OUT = ROOT / "out"
PANDOC_FE = ROOT / "pandoc" / "pandoc_fe.py"

assert PANDOC_FE.is_file(), f"{PANDOC_FE} not found"


def strlist(l):
    return [str(v) for v in l]


def run(*a, **kw):
    sl = strlist(a)
    print("+", " ".join(sl))
    subprocess.run(sl, **kw)


def build_pdf(files, output_dir):
    return run(sys.executable, PANDOC_FE,
               "--output-dir", output_dir,
               "--hush", "--extension", "pdf",
               "--source", *strlist(files))


def capitalize_words(s):
    upper = True
    r = ""
    for c in s:
        if upper:
            r += c.upper()
        else:
            r += c.lower()
        upper = (c == " ")
    return r

def single_file_pretty_name(f):
    fname_raw = f.with_suffix("").name
    fname_pretty = capitalize_words(fname_raw.replace("_", " "))
    return f"{dir_pretty_name(f.parent)} - {fname_pretty}"


def dir_pretty_name(d):
    config_file = d / "course.toml"
    with open(config_file if config_file.exists() else DEFAULT_COURSE_CONFIG) as f:
        cfg = toml.load(f)
    return cfg.get("name", d.name)


class SlidesCLI:
    ENV_ATTRS = ("pretty_name", "sources", "output_dir", "artifacts")

    def __init__(self, path):
        assert path.exists(), f"{path} does not exist"
        fullpath = path.resolve()
        assert COURSES in fullpath.parents, "this script only support courses"
        self.is_lab = (fullpath.name == "labs")

        suffix = " - Labs" if self.is_lab else ""
        if fullpath.is_dir():
            self.files_p = sorted(list(fullpath.glob("*.rst")))
            self.pretty_name = dir_pretty_name(fullpath.parent if self.is_lab else fullpath) + suffix
            self.output_dir = OUT / fullpath.relative_to(COURSES)
        else:
            self.files_p = [fullpath]
            self.pretty_name = single_file_pretty_name(fullpath) + suffix
            self.output_dir = OUT / fullpath.parent.relative_to(COURSES)

        files_as_pdf = (self.output_dir / f.with_suffix(".pdf").name for f in self.files_p)
        self.artifacts = os.linesep.join(strlist(files_as_pdf))
        self.sources = ' '.join(strlist(self.files_p))

        assert list(self.files_p), f"{fullpath}/ does not contain RST files"
        assert all(f.is_file() for f in self.files_p)
        assert all(hasattr(self, k) for k in self.ENV_ATTRS)


    def build(self):
        run(sys.executable, PANDOC_FE,
            "--output-dir", self.output_dir,
            "--hush", "--extension", "pdf",
            "--source", *self.sources.split(' '),
            check=True, text=True)


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("path")
    ap.add_argument("action",
                    choices=["printenv", "build"],
                    default="build",
                    nargs="?")
    args = ap.parse_args()

    sci = SlidesCLI(Path(args.path))
    if args.action == "build":
        sci.build()
    elif args.action == "printenv":
        for k in sci.ENV_ATTRS:
            v = str(getattr(sci, k))
            if os.linesep in v:
                DELIM = "@EOF@"
                assert not DELIM in v, f"BUG: change delimiter here (current is {DELIM!r})"
                print(f"{k.upper()}<<{DELIM}{os.linesep}{v}{os.linesep}{DELIM}")
            else:
                print(f"{k.upper()}={v}")
