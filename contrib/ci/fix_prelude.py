#! /usr/bin/env python3

import os
import sys
from pathlib import Path
import subprocess
import argparse
import hashlib


PROJECT = Path(sys.argv[0]).resolve().parents[2]
CONTRIB = PROJECT / "contrib"

ROLES = (
    ".. role:: ada(code)\n"
    + "    :language: Ada\n"
    + "\n"
    + ".. role:: C(code)\n"
    + "    :language: C\n"
    + "\n"
    + ".. role:: cpp(code)\n"
    + "    :language: C++\n"
    + "\n"
    + ".."
)

SYMBOLS = (
    ".. |rightarrow| replace:: :math:`\\rightarrow`\n"
    + ".. |forall| replace:: :math:`\\forall`\n"
    + ".. |exists| replace:: :math:`\\exists`\n"
    + ".. |equivalent| replace:: :math:`\\iff`\n"
    + ".. |le| replace:: :math:`\\le`\n"
    + ".. |ge| replace:: :math:`\\ge`\n"
    + ".. |lt| replace:: :math:`<`\n"
    + ".. |gt| replace:: :math:`>`\n"
    + ".. |checkmark| replace:: :math:`\\checkmark`\n"
    + "\n"
    + ".."
)

def validate_roles(content):
    return content == ROLES


def validate_symbols(content):
    return content == SYMBOLS


def compare_content(title, actual_str, expected_str):
    retval = []
    actual = actual_str.split("\n")
    expected = expected_str.split("\n")
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
    failures = None
    if interactive:
        failures = []
    else:
        failures = ""
    with open(filename, "r") as file:
        content = file.read()
        pieces = content.split("PRELUDE: ")
        for section in pieces:
            stripped = section.strip()
            name, content = section.split("\n", 1)
            content = content.strip()
            if name == "ROLES":
                if not validate_roles(content):
                    if interactive:
                        failures.extend(compare_content("Roles", content, ROLES))
                    else:
                        failures = failures + " Roles"
            elif name == "SYMBOLS":
                if not validate_symbols(content):
                    if interactive:
                        failures.extend(compare_content("Symbols", content, SYMBOLS))
                    else:
                        failures = failures + " Roles"
    return failures


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument(
        "--files-to-check", type=Path, default=CONTRIB / "rst_files_with_prelude.txt"
    )
    ap.add_argument("--interactive", action="store_true")
    args = ap.parse_args()

    total_failures = 0

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
