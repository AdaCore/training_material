#! /usr/bin/env python3

import os
import sys
from pathlib import Path
import subprocess
import argparse
import hashlib


PRELUDE_FLAG = "container:: PRELUDE "
PROJECT = Path(sys.argv[0]).resolve().parents[2]
CONTRIB = PROJECT / "contrib"
PRELUDE_RST = PROJECT / "support_files" / "prelude.rst"

EXPECTED = {}
ROLES = None
SYMBOLS = None

VALIDATORS = {
    "ROLES": "validate_content",
    "SYMBOLS": "validate_content",
    "PROVIDES": "validate_provides",
}


def load_prelude():
    global EXPECTED

    with open(PRELUDE_RST, "r") as file:
        content = file.read()
        pieces = content.split(PRELUDE_FLAG)
        for section in pieces:
            stripped = section.strip()
            try:
                name, content = section.split("\n", 1)
                EXPECTED[name] = content.strip()
            except:
                pass


def validate_content(name, content):
    global EXPECTED
    return content == EXPECTED[name]


def validate_provides(name, content):
    global EXPECTED
    return True


def compare_content(title, actual_str):
    global EXPECTED
    retval = []
    actual = actual_str.split("\n")
    expected = EXPECTED[title].split("\n")
    l_actual = len(actual)
    l_expected = len(expected)
    length = min([l_actual, l_expected])
    for idx in range(0, length):
        if actual[idx] != expected[idx]:
            retval.append("In " + title)
            retval.append("  Line " + str(idx + 1))
            retval.append("    Actual  : " + actual[idx])
            retval.append("    Expected: " + expected[idx])
    if len(retval) == 0 and l_actual < l_expected:
        retval.append("In " + title + " missing:")
        for idx in range(l_actual, l_expected):
            retval.append("    " + expected[idx])
    elif len(retval) == 0 and l_actual > l_expected:
        retval.append("In " + title + " extra:")
        for idx in range(l_expected, l_actual):
            retval.append("    " + actual[idx])
    return retval


def process_one_file(filename, explain):
    global VALIDATORS

    failures = None

    sections_needed = ["BEGIN", "ROLES", "SYMBOLS", "REQUIRES", "PROVIDES", "END"]

    if explain:
        failures = []
    else:
        failures = ""
    with open(filename, "r") as file:
        content = file.read()
        pieces = content.split(PRELUDE_FLAG)
        for section in pieces:
            stripped = section.strip()
            name, content = section.split("\n", 1)
            try:
                sections_needed.remove(name)
            except:
                pass
            content = content.strip()
            if name in VALIDATORS.keys():
                validator = VALIDATORS[name]
                if not globals()[validator](name, content):
                    if explain:
                        failures.extend(compare_content(name, content))
                    else:
                        failures = failures + " " + name
    if len(sections_needed) > 0:
        if explain:
            failures.append("Missing Section(s)")
            for section in sections_needed:
                failures.append("   " + section)
        else:
            for section in sections_needed:
                failures = failures + " " + section

    return failures


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument(
        "--files-to-check",
        type=Path,
        default=CONTRIB / "rst_files_with_prelude.txt",
        help=(
            "File containing list of files (wildcards allowed). "
            + "Can be relative to the repository ("
            + str(PROJECT)
            + ") "
            "or current directory (if no matches in the repository)"
        ),
    )
    ap.add_argument(
        "--explain",
        help="Give details as to why a file failed the check",
        action="store_true",
    )
    ap.add_argument(
        "--update",
        help="Run 'rst_update_prelude' script to fix errors",
        action="store_true",
    )
    args = ap.parse_args()

    total_failures = 0
    load_prelude()

    with open(args.files_to_check, "rt") as f:
        files_with_prelude_glob = f.read().splitlines()

    for glob in files_with_prelude_glob:
        if len(glob) == 0:
            print("WARN: empty line found in " + str(args.files_to_check))
            continue

        f_prel = list(PROJECT.glob(glob))
        if not f_prel:
            continue

        for one in f_prel:
            failures = process_one_file(one, args.explain)
            if len(failures) > 0:
                total_failures = total_failures + 1
                if args.update:
                    subprocess.check_call(
                        str(sys.executable)
                        + " "
                        + str(os.path.join(CONTRIB, "rst_update_prelude.py"))
                        + " "
                        + "-i "
                        + str(one)
                    )
                elif args.explain:
                    print("FAIL: " + str(one))
                    for line in failures:
                        print("  " + line)
                else:
                    print("FAIL: " + str(one) + failures)

    sys.exit(0 if total_failures == 0 else 1)
