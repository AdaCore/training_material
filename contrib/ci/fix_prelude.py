#! /usr/bin/env python3

import os
import sys
from pathlib import Path
import subprocess
import argparse
import hashlib


PRELUDE_FLAG = "PRELUDE:: "
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


def process_one_file(filename, interactive):
    global VALIDATORS

    failures = None

    sections_needed = ["BEGIN", "ROLES", "SYMBOLS", "REQUIRES", "PROVIDES", "END"]

    if interactive:
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
                    if interactive:
                        failures.extend(compare_content(name, content))
                    else:
                        failures = failures + " " + name
    if len(sections_needed) > 0:
        if interactive:
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
        "--files-to-check", type=Path, default=CONTRIB / "rst_files_with_prelude.txt"
    )
    ap.add_argument("--interactive", action="store_true")
    args = ap.parse_args()

    total_failures = 0
    load_prelude()

    with open(args.files_to_check, "rt") as f:
        files_with_prelude_glob = f.read().splitlines()

    for glob in files_with_prelude_glob:
        f_prel = list(PROJECT.glob(glob))
        if not f_prel:
            continue

        for one in f_prel:
            failures = process_one_file(one, args.interactive)
            if len(failures) > 0:
                total_failures = total_failures + 1
                if args.interactive:
                    print("FAIL: " + str(one))
                    for line in failures:
                        print("  " + line)
                else:
                    print("FAIL: " + str(one) + failures)

    sys.exit(0 if total_failures == 0 else 1)
