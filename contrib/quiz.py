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


def decomment_dedent_lines(s):
    def decomment_dedent_line(ls):
        sls = ls.strip()
        if sls.startswith("--"):
            sls = sls[2:].strip()
        return sls

    return [decomment_dedent_line(l) for l in s.splitlines()]


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
        self.general_explainations = self.adacut(
            "-Km", "answer", out_filter=decomment_dedent_lines
        )

    def adacut(self, *a, **kw):
        return adacut(*a, "--", self.input_file, **kw)


class QuizAnswer:
    @classmethod
    def code_explain_out_filter(cls, s):
        if s.endswith(os.linesep):
            s = s[: -len(os.linesep)]

        code_l = []
        explain_l = []

        explaining = True
        for l in reversed(s.splitlines()):
            ls = l.strip()
            if ls.startswith("--") and explaining:
                explain_l.insert(0, ls[2:].strip())
            else:
                explaining = False
                code_l.insert(0, l)

        if len(code_l) == 1:
            if code_l[0].endswith(";"):
                code_l[0] = code_l[0][:-1]

        return os.linesep.join(code_l), os.linesep.join(explain_l)

    def __init__(self, input_file, i):
        self.identifier = chr(ord("A") + i)
        self.input_file = input_file

        self.code, self.explain = self.adacut(
            "-dc", i + 1, out_filter=self.code_explain_out_filter
        )
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
            print("compiling", self.identifier, file=sys.stderr)
            self.compiles = (
                gprbuild(stdout=subprocess.PIPE, quiet=True, cwd=d).returncode == 0
            )
            if self.compiles:
                print("running", self.identifier, file=sys.stderr)
                main = find_single_executable(d)
                self.runs = main(stdout=subprocess.PIPE, quiet=True).returncode == 0
            else:
                self.runs = False
            epycs.subprocess.exit_on_error = True


def indent(lines):
    return [re.search(r"[^ ]", l).span()[1] - 1 if l.strip() else 0 for l in lines]


def text_indent(min_indent, text):
    lines = text.splitlines()
    actual_min_indent = min(indent(lines))
    if min_indent >= actual_min_indent:

        def indent_line(l):
            return " " * (min_indent - actual_min_indent) + l

    else:

        def indent_line(l):
            # Actually dedent
            return l[actual_min_indent - min_indent :]

    return os.linesep.join(indent_line(l) for l in lines)


def code_as_text(code, answer, pre_code_indent=3):
    lines_raw = code.splitlines()
    lines_indent_raw = indent(lines_raw)
    min_lines_indent = min(lines_indent_raw)
    corrected_lines_indent = [i - min_lines_indent for i in lines_indent_raw]
    lines = [l.strip() for l in lines_raw]

    if answer:

        def wrap(s):
            return f":answermono:`{s}`"

    else:

        def wrap(s):
            return f"``{s}``"

    if len(lines) > 1:
        return (os.linesep + (" " * pre_code_indent)).join(
            "| " + " " * corrected_lines_indent[i] + wrap(l)
            for i, l in enumerate(lines)
        )
    else:
        return wrap(lines[0])


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("input_file", type=Path)
    ap.add_argument("-o", "--output-file")
    ap.add_argument(
        "-t",
        "--add-title",
        action="store_true",
        help="Decorate the resulting rst with a Quiz title",
    )
    args = ap.parse_args()

    if args.output_file:
        out = open(args.output_file, "w")
    else:
        out = sys.stdout

    def pout(*a, file=out, **kw):
        print(*a, file=file, **kw)

    assert args.input_file.is_file()

    quiz = Quiz(args.input_file)

    if args.add_title:
        pout("------")
        pout("Quiz")
        pout("------")
        pout()

    if quiz.code_question:
        pout(".. code:: Ada")
        pout()
        pout(text_indent(4, quiz.code_question))
        pout()

    if quiz.question:
        pout(quiz.question)
        pout()

    explainations = quiz.general_explainations
    for i in range(quiz.answers_number):

        answer = QuizAnswer(args.input_file, i)

        pout(f"{answer.identifier}.", code_as_text(answer.code, answer=answer.runs))
        if answer.explain:
            explainations.append(f"{answer.identifier}. {answer.explain}")

    if explainations:
        pout()
        pout(".. container:: animate")
        pout()
        pout((" " * 4) + (os.linesep + (" " * 4)).join(explainations))
