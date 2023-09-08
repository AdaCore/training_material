import argparse
from pathlib import Path
import difflib
import os
import sys


PROJECT = Path(__file__).resolve().parents[1]
COURSES = PROJECT / "courses"


def fix_titles(s):
    TEXT, SEP, TITLE = range(3)
    st = TEXT

    lout = []

    for l in s:
        line_is_title = (
            len(l) > 1 and l[0] in "-=*" and all(c == l[0] for c in l.rstrip())
        )
        lout.append(l)
        if st == TEXT:
            if line_is_title:
                st = SEP
        elif st == SEP:
            if line_is_title:
                # Fake title start
                lout = lout[:-2]
                st = TEXT
            else:
                if len(lout[-2]) < len(l):
                    # Fix opening separator
                    lout[-2] = lout[-2][0] * len(l)
                st = TITLE
        elif st == TITLE:
            if line_is_title:
                if lout[-1].strip() != lout[-3].strip():
                    # fix title by copying the previous one
                    lout[-1] = lout[-3]
            else:
                # drop title
                lout = lout[:-3]
            st = TEXT

    return lout


def get_diff(f, before, after):
    return os.linesep.join(
        difflib.unified_diff(
            before, after, fromfile=f"a/{f.name}", tofile=f"b/{f.name}"
        )
    )


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("files", nargs="*", default=COURSES.glob("**/*.rst"), type=Path)
    ap.add_argument(
        "--check",
        "-c",
        action="store_true",
        help="""Don't write the files back, just return the status.
Return code 0 means nothing would change.
Return code 1 means some files would be reformatted.
Return code 123 means there was an internal error.""",
    )
    args = ap.parse_args()

    diff = False
    for f in args.files:
        with open(f) as fd:
            before = fd.read().splitlines()

        after = fix_titles(before)

        if before != after:
            print(f)
            diff = True
            print(get_diff(f, before, after))

            if not args.check:
                with open(f, "wt") as fd:
                    fd.write(os.linesep.join(after))

    sys.exit(1 if diff else 0)
