"""
This is a simple wrapper around the "pandoc_fe.py" script so
you don't have to specify everything it needs to be fully flexible.

    --course COURSE
        text file listing all RST modules to put in document

    --output OUTPUT       
        basename of output file (use course name if not specified)

    --extension EXTENSION
        Output file extension. "PDF" => Beamer, "TEX" => LaTeX,
        "DOCX" => Word, "PPTX" => PowerPoint

    --hide
        Do not open created file when done. If not set, the PDF/DOCX
        file should open automatically
"""

import argparse
import os
import os.path
import sys
import subprocess
import pathlib


def material(stuff):
    """
    Find the location of the material repo based on the current location.
    stuff - list of items to be appended to the material repo folder
    """
    for lvl in range(0, 4):
        retval = str(pathlib.Path.cwd().resolve().parents[lvl])
        if retval.endswith("material"):
            for one in stuff:
                retval = os.path.join(retval, one)
            return retval
    print("Could not find 'material' repo")
    os.exit(-1)


def PYTHON():
    """
    Hack to get the correct python executable
    """
    if "linux" in sys.platform:
        return "python3 "
    else:
        return "python "


if __name__ == "__main__":

    parser = argparse.ArgumentParser()

    parser.add_argument("--course", help="course TXT file", required=True)

    parser.add_argument(
        "--output",
        help="basename of output file (use course name if not specified)",
        default=None,
    )

    parser.add_argument(
        "--extension",
        help='Output file extension. "PDF" => Beamer, "TEX" => LaTeX, "DOCX" => Word, "PPTX" => PowerPoint',
        default="pdf",
        required=False,
    )

    parser.add_argument(
        "--hide", help="Do not open created file when done", action="store_true"
    )

    args = parser.parse_args()

    output_filename = ""
    if args.output == None:
        output_filename = os.path.splitext(args.course)[0]
    else:
        output_filename = args.output

    if os.path.isfile(output_filename):
        try:
            os.remove(output_filename)
        except:
            print("'" + output + "' in use - close it or choose a different name")
            os.exit(-1)

    command = (
        PYTHON()
        + material(["pandoc", "pandoc_fe.py"])
        + " --source "
        + os.path.abspath(args.course)
        + " --output "
        + output_filename
        + " --extension "
        + args.extension
        + " --directories "
        + material(["images"])
        + ","
        + material(["support_files"])
        + " --theme adacore --color adacore "
        + " --filter "
        + material(["pandoc", "beamer_filter.py"])
    )
    print(command)
    os.system(command)
    if not args.hide:
        os.system("start " + output_filename + "." + args.extension)
