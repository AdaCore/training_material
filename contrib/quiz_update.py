#! /usr/bin/env python3

import os
import sys
import argparse
from pathlib import Path
import subprocess
import epycs.subprocess
from epycs.subprocess import cmd
import difflib


CONTRIB = Path(__file__).resolve().parent

QUIZ_SCRIPT = CONTRIB / "quiz.py"

quiz = cmd.python.arg(QUIZ_SCRIPT).with_default_kw(out_filter=str)

if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("-v", "--verbose", action="store_true")
    ap.add_argument(
        "-u",
        "--update",
        action="store_true",
        help="in case of failure, update the quiz",
    )
    ap.add_argument("--quiz-file-name", default="quiz.rst")
    ap.add_argument("-C", "--no-check", action="store_true")
    ap.add_argument("main_directory", type=Path)
    args = ap.parse_args()

    epycs.subprocess.verbose = args.verbose
    epycs.subprocess.exit_on_error = False

    check_failed = False

    if (args.main_directory / args.quiz_file_name).is_file():
        quizzes = [args.main_directory / args.quiz_file_name]
    else:
        quizzes = list(sorted(args.main_directory.glob(f"**/{args.quiz_file_name}")))
    assert len(quizzes) != 0

    for f in quizzes:
        d = f.parent
        print(d.name, end=": ")
        dtpl = d / "template"
        try:
            assert dtpl.is_dir(), f"expected a template dir in {d}"
            ing = list(dtpl.glob("*.adb"))
            assert len(ing) == 1, f"expected a single adb file in {dtpl} got {ing}"
            inf = ing[0]

            with open(f) as fread:
                q_old = fread.read()

            stderr = None if args.verbose else subprocess.DEVNULL
            q_new = quiz(inf, stderr=stderr, check=True, out_filter=str)

            if q_old != q_new:
                print("\033[31mFAIL\033[0m")
                check_failed = True
                diff_lines = list(
                    difflib.unified_diff(
                        q_old.splitlines(),
                        q_new.splitlines(),
                        fromfile=f"{d.name} (old)",
                        tofile=f"{d.name} (new)",
                    )
                )

                pr = lambda s: None
                color = lambda c: "31" if c == "-" else ("32" if c == "+" else "0")
                print_colored = lambda s: print(f"\033[{color(s[0])}m{s}\033[0m")
                for i, l in enumerate(diff_lines):
                    if l:
                        pr(l)
                    if l.startswith("@") or l.startswith("@"):
                        diff_lines = diff_lines[i + 1 :]
                        pr = print_colored
                print()

                if args.update:
                    with open(f, "wt") as fwrite:
                        print(q_new, file=fwrite, end="")
            else:
                print("\033[32mOK\033[0m")
        except subprocess.CalledProcessError as e:
            print(f"\033[1;41mERROR\033[0m quiz.py {e}")
            raise
        except Exception as e:
            print(f"\033[1;41mERROR\033[0m {e.__class__.__name__} {e}")
            raise
        except:
            import traceback

            print(f"\033[1;41mERROR\033[0m unknown error")
            print(traceback.format_exc())
            raise

    if not args.no_check and check_failed:
        sys.exit(1)
