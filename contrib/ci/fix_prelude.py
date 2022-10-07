#! /usr/bin/env python3

import os
import sys
from pathlib import Path
import subprocess
import argparse
import hashlib


PROJECT = Path(sys.argv[0]).resolve().parents[2]
CONTRIB = PROJECT / "contrib"


def rst_update_prelude(files):
    subprocess.check_call(
        str(s)
        for s in [sys.executable, CONTRIB / "rst_update_prelude.py", "-i"] + files
    )


def files_digest(files):
    h = hashlib.sha256()
    for f in sorted(files):
        with open(f, "rb") as frd:
            h.update(frd.read())
    return h.digest()


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument(
        "--files-to-check", type=Path, default=CONTRIB / "rst_files_with_prelude.txt"
    )
    ap.add_argument("--no-digests-check", "-C", action="store_true")
    args = ap.parse_args()

    check_digest = not args.no_digests_check
    digests_have_changed = False

    with open(args.files_to_check, "rt") as f:
        files_with_prelude_glob = f.read().splitlines()

    for glob in files_with_prelude_glob:
        f_prel = list(PROJECT.glob(glob))
        if not f_prel:
            continue

        print(glob)
        if check_digest:
            before = files_digest(f_prel)

        rst_update_prelude(f_prel)
        if check_digest and before != files_digest(f_prel):
            print(f"{glob}: files didn't have the proper prelude", file=sys.stderr)
            print(f"run {Path(sys.argv[0]).name} locally and commit the results to fix")
            digests_have_changed = True

    if check_digest:
        sys.exit(0 if not digests_have_changed else 1)
