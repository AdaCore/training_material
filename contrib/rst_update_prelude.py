import os
import sys
import argparse
from pathlib import Path


CWD = Path(sys.argv[0]).resolve().parents[1]


def section_start(l):
    return len(l) > 2 and l[0] == l[1] and l[1] == l[2] and l[0] in "*-=+"


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--prelude", default=CWD / 'support_files' / 'prelude.rst')
    ap.add_argument("--in-place", "-i", action='store_true')
    ap.add_argument("files", nargs="+")
    args = ap.parse_args()

    with open(args.prelude, 'rt') as pf:
        prelude_content = pf.read()

    if prelude_content.endswith(os.linesep):
        prelude_content = prelude_content[:-1]

    for f in args.files:
        with open(f, 'rt') as ff:
            file_lines = ff.read().splitlines()

        f_out = open(f, 'wt') if args.in_place else sys.stdout

        def print_out(*a, **kw):
            kw.setdefault("file", f_out)
            print(*a, **kw)
    
        STATE_INIT, STATE_CHAPTER, STATE_END = 0, 1, 2
        state = STATE_INIT
        skip = 0
        for l in file_lines:
            if skip:
                skip -= 1
                print_out(l)
            elif state == STATE_INIT:
                if section_start(l):
                    state = STATE_CHAPTER
                    skip = 3
                    print_out(l)
            elif state == STATE_CHAPTER:    
                if section_start(l):
                    print_out(prelude_content)
                    print_out()
                    print_out(l)
                    state = STATE_END
            elif state == STATE_END:
                print_out(l)
