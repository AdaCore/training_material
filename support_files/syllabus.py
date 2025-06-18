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


def run_pandoc(format, rst_file):

    reference = ""
    output_filename = os.path.splitext(rst_file)[0] + "." + format

    if format == "docx":
        reference = (
            "--reference-doc "
            + os.path.splitext(os.path.abspath(__file__))[0]
            + ".dotm "
        )

    command = (
        "pandoc -f rst -t "
        + format
        + " "
        + "-o "
        + output_filename
        + " "
        + reference
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

    return output_filename


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


def read_content(in_filename, lines):
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
            read_content(os.path.abspath(full_path), lines)
        lines.append(line)

    file.close()


def generate_docx(modules, rst_filename, title):

    fp = open(rst_filename, "w")

    fp.write(header(title, TITLE))
    for module in modules:
        name = module[0]
        values = module[1]
        if name.startswith("--"):
            fp.write(header(name[2:], BREAK))
        else:
            fp.write(header(name, MODULE))
            for value in values:
                fp.write(header(value, SECTION))

    fp.close()
    output_filename = run_pandoc("docx", rst_filename)
    os.system("start " + output_filename)


def generate_html(modules, rst_filename, left):

    fp = open(rst_filename, "w")

    fp.write(".. list-table::\n")
    fp.write("   :header-rows: 1\n")
    fp.write("\n")

    fp.write("   * - " + left + "\n")
    fp.write("     - Topic\n")

    front = "     - | "
    for module in modules:
        name = module[0]
        values = module[1]
        if name.startswith("--"):
            fp.write("   * - " + name[2:] + "\n")
            front = "     - | "
        else:
            fp.write(front + name + "\n")
            front = "       | "

    fp.close()
    html_filename = run_pandoc("html", rst_filename)
    os.system("type " + rst_filename)


def load_one_module(module_filename, short):

    content = []
    read_content(module_filename, content)

    title = ""
    sections = []

    for i in range(2, len(content)):
        flag, name = get_title(content[i - 2 : i + 1])
        if flag == MODULE:
            title = name
        elif flag == SECTION:
            if want_section(name.lower()) and not short:
                sections.append(name)

    return (title, sections)


def load_modules(course, short):
    """
    Load modules from course file.
    Return a list of tuples, where the first element is the module
    name and the second element is a list of chapters.
    If "short" is True, the list of chapters will be empty
    """

    all_modules = []
    with open(course) as f:
        filenames = f.read().splitlines()

        for f in filenames:
            if f.startswith("--"):
                separator = ("--" + f[2:].strip(), [])
                all_modules.append(separator)
            elif f.startswith("#"):
                separator = ("--" + f[1:].strip(), [])
                all_modules.append(separator)
            else:
                all_modules.append(load_one_module(os.path.abspath(f), short))

    return all_modules


if __name__ == "__main__":

    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--course", help="file containing list of RST files in course", required=True
    )

    parser.add_argument("--rst", help="RST file output", required=True)

    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "--html",
        action="store_true",
        help="Generate a table in an RST file for use in displaying a schedule",
    )
    group.add_argument(
        "--docx",
        action="store_true",
        help="Generate an RST file (and convert to Word) for use in generating a syllabus",
    )

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

    parser.add_argument(
        "--left",
        help='Title of left column in table (default="Session")',
        default="Session",
    )

    args = parser.parse_args()

    rst_file = os.path.abspath(args.rst)
    docx_file = os.path.splitext(rst_file)[0] + ".docx"
    reference_dotm = os.path.splitext(os.path.abspath(__file__))[0] + ".dotm"

    all_modules = load_modules(args.course, args.short)

    if args.docx:
        generate_docx(all_modules, args.rst, args.title)
    elif args.html:
        generate_html(all_modules, args.rst, args.left)
