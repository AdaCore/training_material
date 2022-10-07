#!/usr/bin/env python3

from argparse import ArgumentParser
from pathlib import Path
from subprocess import run
import sys


ROOT = Path(sys.argv[0]).parent.resolve()
CONTRIB = ROOT.parents[3] / "contrib"


def adacut(f, answer=True):
    a = [str(CONTRIB / "adacut.py"), "-m", "answer" if answer else "question", str(f)]
    r = run(a, text=True, capture_output=True)

    if r.returncode:
        print("fail")
        print(" ".join(a))
        print(r.stderr, file=sys.stderr)
        sys.exit(1)
    else:
        with open(f.parents[1] / ("answers" if answer else "src") / f.name, "wt") as d:
            d.write(r.stdout)


def gprclean(gpr):
    run(["gprclean", "-P", gpr], check=True)


def gprbuild(gpr, **kw):
    run(["gprbuild", "-C", gpr] + [f"-X{k}={v}" for (k, v) in kw.items()], check=True)


if __name__ == "__main__":
    for gpr in ROOT.glob("**/*.gpr"):
        for tpl in gpr.parent.glob("template/*.ad?"):
            adacut(tpl, True)
            adacut(tpl, False)

        gprclean(gpr)
        gprbuild(gpr, Mode="Problem")
        run(str(gpr.parent / "obj" / "main"), check=True)
        gprclean(gpr)
        gprbuild(gpr, Mode="Solution")
        run(str(gpr.parent / "obj" / "main"), check=True)
