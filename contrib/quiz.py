#!/usr/bin/env python3
import argparse
import re
import sys
import os
import stat
import tempfile
from epycs.subprocess import cmd
import epycs.subprocess
import subprocess
from pathlib import Path
import shutil

epycs.subprocess.verbose = True

SCRIPTS = Path(__file__).parent
ADACUT_PY = SCRIPTS / "adacut.py"
assert ADACUT_PY.is_file()

adacut = cmd.python.arg(ADACUT_PY)
gprbuild = cmd.gprbuild


def is_executable(f):
    return 0 != os.stat(f)[stat.ST_MODE] & (stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)


def find_single_executable(d):
    x = None
    with open("/tmp/find.txt", "wt") as fout:
        for dd, _, files in os.walk(d):
            for f in files:
                ffull = Path(dd) / f
                print(str(ffull), file=fout)
                if is_executable(ffull):
                    assert not x
                    x = ffull
    assert x
    return epycs.subprocess.find_program(x.resolve())


class Quiz:
    def __init__(self, input_file):
        self.input_file = input_file

        with open(self.input_file) as f:
            self.full_code = f.read()

        self.answers_number = self.adacut("-C", out_filter=int)
        first_line = self.full_code[: self.full_code.find(os.linesep)].strip()
        if first_line.startswith("-- "):
            self.question = first_line[2:].strip()
        else:
            self.question = None
        self.code_question = self.adacut("-Km", "question", out_filter=str)

    def adacut(self, *a, **kw):
        return adacut(*a, "--", self.input_file, **kw)


class QuizAnswer:
    @classmethod
    def code_out_filter(cls, s):
        if s.endswith(os.linesep):
            s = s[: -len(os.linesep)]
        if s.endswith(";"):
            s = s[:-1]

        return s

    def __init__(self, input_file, i):
        self.input_file = input_file

        self.code = self.adacut("-dc", i + 1, out_filter=self.code_out_filter)
        self.full_code = self.adacut("-kc", i + 1, out_filter=str)

        self.check_full_code()

    def adacut(self, *a, **kw):
        return adacut(*a, "--", self.input_file, **kw)

    def check_full_code(self):
        # build a temporary project with the answer
        with tempfile.TemporaryDirectory() as d:
            origin = self.input_file.parents[1]
            shutil.copytree(origin, d, dirs_exist_ok=True)
            (Path(d) / "src").mkdir(exist_ok=True)
            with open(Path(d) / "src" / self.input_file.name, "wt") as f:
                f.write(self.full_code)
            print(Path(d) / "src" / self.input_file.name, file=sys.stderr)

            epycs.subprocess.exit_on_error = False
            self.compiles = (
                gprbuild(stdout=subprocess.PIPE, quiet=True, cwd=d).returncode == 0
            )
            if self.compiles:
                main = find_single_executable(d)
                self.runs = main(stdout=subprocess.PIPE, quiet=True).returncode == 0
            else:
                self.runs = False
            epycs.subprocess.exit_on_error = True


def text_indent(min_indent, text):
    lines = text.splitlines()
    actual_min_indent = min(re.search(r"[^ ]", l).span()[1] - 1 for l in lines)
    if min_indent >= actual_min_indent:

        def indent_line(l):
            return " " * (min_indent - actual_min_indent) + l

    else:

        def indent_line(l):
            # Actually dedent
            return l[actual_min_indent - min_indent :]

    return os.linesep.join(indent_line(l) for l in lines)


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("input_file", type=Path)
    ap.add_argument("-o", "--output-file")
    args = ap.parse_args()

    if args.output_file:
        out = open(ap, "w")
    else:
        out = sys.stdout

    assert args.input_file.is_file()

    quiz = Quiz(args.input_file)

    print("------")
    print("Quiz")
    print("------")
    print()
    if quiz.code_question:
        print(".. code:: Ada")
        print()
        print(text_indent(4, quiz.code_question))
        print()

    if quiz.question:
        print(quiz.question)
        print()

    for i in range(quiz.answers_number):

        answer = QuizAnswer(args.input_file, i)

        if not answer.runs:
            print(f"{chr(ord('A') + i)}. ``{answer.code}``", file=out)
        else:
            print(f"{chr(ord('A') + i)}. :answermono:`{answer.code}`", file=out)
