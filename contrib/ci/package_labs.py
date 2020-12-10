#!/usr/bin/env python3
import argparse
import subprocess
from pathlib import Path
import sys

CI_DIR = Path(sys.argv[0]).resolve().parent
SCRIPT_DIR = CI_DIR.parent
ROOT_DIR = SCRIPT_DIR.parent
PANDOC_DIR = ROOT_DIR / "pandoc"
OUT_DIR = ROOT_DIR / "out"

verbose = True

def check_call(*args, **kw):
    al = [str(s) for s in args]
    if verbose:
        print(" ".join(al))
    subprocess.check_call(al, **kw)

def python(script, *args, **kw):
    assert script.is_file()
    check_call(sys.executable, script, *args, **kw)

def generate_labs_docs(labs_dir, out_dir):
    python(PANDOC_DIR / "lab_docs.py", "-r", labs_dir, out_dir)

def bash(script, *args, **kw):
    assert script.is_file()
    check_call("bash", script, *args, **kw)

def package_labs(labs_dir, out_dir):
    bash(CI_DIR / "package_labs.sh", labs_dir, out_dir)

if __name__ == "__main__":
    ap = argparse.ArgumentParser( "Generate and package the lab sources and "
                                + "PDF file together")
    ap.add_argument("labs_dir", help="Directory containing one or several labs",
                    type=Path)
    ap.add_argument("out_dir", nargs="?", type=Path)
    args = ap.parse_args()

    labs_dir = args.labs_dir
    assert labs_dir.is_dir()

    out_dir = args.out_dir or OUT_DIR / labs_dir.name
    out_dir.mkdir(parents=True, exist_ok=True)
    assert out_dir.is_dir()

    generate_labs_docs(labs_dir, out_dir)
    package_labs(labs_dir, out_dir)
