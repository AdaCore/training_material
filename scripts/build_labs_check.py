import sys
from pathlib import Path
import subprocess
import argparse

def gprbuild(*args):
    called = ["gprbuild"] + [str(a) for a in args]

    print(" ".join(called))
    subprocess.check_call(called)

if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("search_path", type=Path, default=Path(sys.argv[0]).parent)
    args = ap.parse_args()

    run = 0
    failures = 0

    for p in args.search_path.glob("**/*.gpr"):
        run += 1
        try:
            gprbuild("-v", "-XMode=Solution", p)
            print("Success")
        except subprocess.CalledProcessError:
            failures += 1
            print("Failed", file=sys.stderr)

    if run:
        print(f"{run} run, {failures} errors")
    else:
        print("Nothing to run", file=sys.stderr)
    sys.exit(0 if run and not failures else 1)
