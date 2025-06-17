"""
This script is designed to create a syllabus based on a text
file used to create slides from the material/courses repository
(e.g. "standard_course.txt").

It will generate an RST file, then uses pandoc to convert to a
Word document. It uses a reference document that would use the
same full path to this script, replacing ".py" with ".dotm".

Switches:
   --course => filename containing list of RST files in the
               current directory
   --rst    => name of the RST file that will contain the
               text to convert to a WORD document
   --short  => If set, the output file will only contain
               module names. Default behavior is to create
               a file with module names and sections within
               the module
   --title  => Default course title is "Ada Essentials". Specify
               something else here if you want a different title.

Due to limitations converting from RST to DOCX, the ouput document
needs some editing. This script will print those steps at the
end of processing.
"""

import argparse
import copy
import os
import subprocess

TITLE = "+++"
BREAK = "---"
MODULE = "***"
SECTION = "==="

DEFAULT_TITLE = "Ada Essentials"


def get_title(lines):
    """
    Based on our RST formatting, a module or section title
    uses a particular separator on on the line before and after
    the title.
    If the "lines" parameter follows this format, we return a
    tuple of a flag indicating if we've found a module or a section, and
    the title of the module/section
    """
    if lines[2].startswith(MODULE) and lines[0].startswith(MODULE):
        return MODULE, lines[1]
    elif lines[2].startswith(SECTION) and lines[0].startswith(SECTION):
        return SECTION, lines[1]
    else:
        return "", ""


def want_section(title):
    """
    We don't want to include these sections in our syllabus
    """
    return title != "lab" and title != "introduction" and title != "summary"


def header(title, which):
    """
    Generate a header for the text. We use headers instead of lists because
    they translate to "Heading #" styles in Word, which we can pre-format
    in the reference document.
    """
    separator = (2 + len(title)) * which[0]
    return title + "\n" + separator + "\n\n"


def append_content(in_filename, lines):
    """
    Used to process the "include" directive in an RST file so that
    we just end up with one big file.
    """
    file = open(in_filename)

    while True:
        line = file.readline()
        if not line:
            break
        line = line.strip()
        inc = line.find("include::")
        if inc > 0:
            included = line[13:]
            full_path = os.path.join(os.path.dirname(in_filename), included)
            append_content(os.path.abspath(full_path), lines)
        lines.append(line)

    file.close()


def process_one_file(fp, in_filename, short):
    """
    Read an RST file and print out the module title and,
    if "short" is False, print out the section titles
    """

    lines = []
    append_content(in_filename, lines)

    for i in range(2, len(lines)):
        flag, title = get_title(lines[i - 2 : i + 1])
        if flag == MODULE:
            fp.write(header(title, MODULE))
        elif flag == SECTION:
            if want_section(title.lower()) and not short:
                fp.write(header(title, SECTION))


def create_syllabus(course, rst_filename, short, title):
    """
    Main routine to create the syllabus document
    """

    fp = open(rst_filename, "w")

    fp.write(header(title, TITLE))

    filenames = None
    with open(course) as f:
        filenames = f.read().splitlines()

    for f in filenames:
        if f.startswith("--"):
           # If the line in the file starts with a comment,
           # then this is a break (e.g. "-- Day 1" or "-- Monday AM")
           title = f[2:].strip()
           if len(title) > 0:
               fp.write(header(title, BREAK))

        else:
            process_one_file(fp, os.path.abspath(f), short)

    fp.close()


if __name__ == "__main__":

    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--course", help="file containing list of RST files in course", required=True
    )

    parser.add_argument("--rst", help="RST file output", required=True)

    parser.add_argument(
        "--short",
        help="Just print modules (print sections if not set)",
        action="store_true",
    )

    parser.add_argument(
        "--title",
        help='Syllabus title (default="' + DEFAULT_TITLE + '")',
        default=DEFAULT_TITLE,
    )

    args = parser.parse_args()

    rst_file = os.path.abspath(args.rst)
    docx_file = os.path.splitext(rst_file)[0] + ".docx"
    reference_dotm = os.path.splitext(os.path.abspath(__file__))[0] + ".dotm"

    create_syllabus(args.course, args.rst, args.short, args.title)

    command = (
        "pandoc -f rst -t docx "
        + "-o "
        + docx_file
        + " "
        + "--reference-doc "
        + reference_dotm
        + " "
        + rst_file
    )

    process = subprocess.Popen(command, shell=False, stdout=subprocess.PIPE)

    while True:
        output = process.stdout.readline()
        if process.poll() is not None:
            break
        if output:
            text = output.decode().strip()
            print(text)
    process.wait()

    os.system("start " + docx_file)
    print("Output file is here: " + docx_file)
    print("You still need to edit it as follows:")
    print("  1) Click at the beginning of the first module and")
    print("     create a new section by clicking")
    print("     Layout => Breaks => Section Breaks => Continuous")
    print("  2) Click somewhere after the break, and change to two columns")
    print("     by clicking Layout => Columns => Two")
    print("  3) You will need to insert column breaks immediately before any")
    print("     module that spans multiple columns by clicking")
    print("     by clicking Layout => Breaks => Column")
