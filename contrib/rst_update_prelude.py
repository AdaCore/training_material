"""
Currently, every module needs to contain a 'prelude' that
is expected to have the same content across every module.
This script is designed to "fix" the prelude when
a required content is updated / added.

To better organize this content, the default prelude
file uses RST containers to indicate where each specific
content begins. These sections are as follows:
   BEGIN - any beginning matter
   ROLES - common RST roles
   SYMBOLS - common symbol replacements
   REQUIRES - module-specific content
   PROVIDES - module-specific content
   END - end of prelude (contains the rest of the module).
In addition, anything appearing before BEGIN will be
saved as well.

The general process is to use 'split' to break
the module into sections and use the prelude section
name as the key. Anything appearing before BEGIN
will use FIRST_KEY as the key. We then compare the
actual to expected content only for BEGIN, SYMBOLS,
and ROLES).  If there is a difference, the actual
content is updated. (The remaing sections are considered
module-specific).
After comparison, the sections are written out in the
same order they were read in (either to stdout or
back to the original file).
"""

import os
import sys
import argparse
from pathlib import Path


CWD = Path(sys.argv[0]).resolve().parents[1]

"""
Flag used to split file into pieces
"""
PRELUDE_FLAG = "container:: PRELUDE "

FIRST_KEY = "000"

"""
Dictionary used to store expected prelude pieces
"""
EXPECTED = {}


"""
Populate the expected values from the predefined prelude
"""


def load_prelude(filename):
    global EXPECTED

    with open(filename, "rt") as file:
        content = file.read()
        pieces = content.split(PRELUDE_FLAG)
        EXPECTED[FIRST_KEY] = pieces[0]
        for section in pieces[1:]:
            name, content = section.split("\n", 1)
            EXPECTED[name] = content


"""
Compare 'filename' contents to the expected contents.
If no changes are necessary, the file is not modified
"""


def process_file(filename, in_place):

    ACTUAL = {}
    pieces = None
    update_required = False

    with open(filename, "rt") as file:
        content = file.read()
        pieces = content.split(PRELUDE_FLAG)
        ACTUAL[FIRST_KEY] = pieces[0]
        for section in pieces[1:]:
            name, content = section.split("\n", 1)
            ACTUAL[name] = content

    for key in ACTUAL.keys():
        if not key in EXPECTED.keys():
            if in_place:
                print("   removing " + key)
            del ACTUAL[key]
            update_required = True

        elif key == "BEGIN" or key == "ROLES" or key == "SYMBOLS":
            if ACTUAL[key] != EXPECTED[key]:
                if in_place:
                    print("   updating " + key)
                ACTUAL[key] = EXPECTED[key]
                update_required = True

    for key in EXPECTED.keys():
        if not key in ACTUAL.keys():
            if in_place:
                print("   adding " + key)
            ACTUAL[key] = EXPECTED[key]
            update_required = True

    if update_required:
        f_out = open(filename, "wt") if in_place else sys.stdout
        for key in ACTUAL.keys():
            if key == FIRST_KEY:
                f_out.write(ACTUAL[key])
            else:
                f_out.write(PRELUDE_FLAG + key + "\n" + ACTUAL[key])


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--prelude", default=CWD / "support_files" / "prelude.rst")
    ap.add_argument("--in-place", "-i", action="store_true")
    ap.add_argument("files", nargs="+")
    args = ap.parse_args()

    load_prelude(args.prelude)

    for f in args.files:
        process_file(f, args.in_place)
