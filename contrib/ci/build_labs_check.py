import sys
import platform
from pathlib import Path
import subprocess
import argparse
import enum
import re

verbose = False

class VT_Color(enum.Enum):
   RED = 31
   GREEN = 32
   YELLOW = 33
   BLUE = 34

def print_color(color, *args, **kw):
    print("\033[{}m{}\033[0m".format(
            str(color.value),
            " ".join(str(a) for a in args)),
          **kw)

def info(*args):
    print_color(VT_Color.BLUE, *args)

def error(*args):
    print_color(VT_Color.RED, *args, file=sys.stderr)

def check_call(prog, *args):
    called = [prog] + [str(a) for a in args]
    if verbose:
        print("$", " ".join(called))

    run_kw = {
        "check" : verbose,
        "capture_output" : not verbose,
        "text": True,
    }

    r = subprocess.run(called, **run_kw)
    if not verbose:
        # Manually check and raise
        if r.returncode != 0:
            print("$", " ".join(called))
            print(r.stdout)
            print(r.stderr, sys.stderr)
        r.check_returncode()

def gprbuild(*args):
    check_call("gprbuild", *args)

def gprclean(*args):
    check_call("gprclean", *args)


if __name__ == "__main__":
    ap = argparse.ArgumentParser("Build recursively all gpr file from the given directory and " +
                                 "stop at the first compilation error.\n" +
                                 "\n" +
                                 "By default the build will run twice:\n" +
                                 " * Once with the Mode scenario variable set to Problem\n" +
                                 " * Once with the Mode scenario variable set to Solution\n" +
                                 "\n" +
                                 "It can be configured by creating files alongside the gpr file\n" +
                                 "* .ci_build_skip : skip the build for this project.\n" +
                                 "* .ci_build_skip_problem : skip the problem mode build.\n" +
                                 "\n" +
                                 "This allows to manage labs that won't compile easily\n" +
                                 "\n" +
                                 "Warning: Labs need a proper env, set by calling\n"
                                 +"$ source contrib/ci/lab_env.sh")
    ap.add_argument("-v", "--verbose", action="store_true",
                    help="Print all calls as they are performed, otherwise print only errors")
    ap.add_argument("search_path", type=Path,
                    nargs='?', default=Path(sys.argv[0]).parent)
    args = ap.parse_args()

    verbose = args.verbose

    host = platform.system()

    run = 0
    skip = 0
    failures = 0

    for p in sorted(args.search_path.glob("**/*.gpr")):
        info(p)
        run += 2
        if (p.parent / '.ci_build_skip').exists():
            skip += 2
            print_color(VT_Color.YELLOW, "Skip")
            continue

        try:
            if (p.parent / '.ci_build_skip_problem').exists():
                skip += 1
                print_color(VT_Color.YELLOW, "Skip problem")
            else:
                info("Build problem")
                gprbuild("-v", "-XMode=Problem", f"-XHOST={host}", p)
                gprclean(p)
            info("Build solution")
            gprbuild("-v", "-XMode=Solution", f"-XHOST={host}", p)
            print_color(VT_Color.GREEN, "Success")
        except subprocess.CalledProcessError:
            failures += 1
            error("Failed")

    if run:
        print(f"{run} run, {skip} skipped, {failures} errors")
    else:
        print("Nothing to run", file=sys.stderr)
    sys.exit(0 if run and not failures else 1)
