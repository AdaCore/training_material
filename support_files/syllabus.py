"""
This script is designed to create a syllabus based on a text
file used to create slides from the material/courses repository
(e.g. "standard_course.txt").

(Note: If there are comments in the TXT file, they will be use
to break the modules into sections.)

It will generate an RST file, then uses pandoc to convert to a
Word document. It uses a reference document that would use the
same full path to this script, replacing ".py" with ".dotm".

Switches:
   --course => filename containing list of RST files in the current
               directory
   --rst    => name of the RST file that will contain the generated
               RST content
   --docx   => RST file will be formatted for conversion to a Word
               document (used for generating a course syllabus).
               A ".docx" file will also be created - you will
               probably need to clean it up before sending to the
               customer.
   --html   => RST file will be formatted to describe the course
               schedule. This file should be added to the
               "public-training-website" repository in the
               appropriate location.

The following switches are only useful when selecting "--docx"
   --short  => If set, the output file will only contain
               module names. Default behavior is to create
               a file with module names and sections within
               the module (ignored when using
   --title  => Default course title is "Ada Essentials". Specify
               something else here if you want a different title.

The following switch is only useful when selecting "--html"

   --left   => Default column title for left column is "Session".
               Specify something else here if you want a different
               title. (Right column is always "Topic")
"""

import argparse
import copy
import os
import platform
import subprocess

TITLE = "+++"
BREAK = "---"
MODULE = "***"
SECTION = "==="

DEFAULT_TITLE = "Ada Essentials"


def run_pandoc(format, rst_file):
    """
    Run 'pandoc' to generate the specified format from the rst_file.
    More useful for 'docx' than 'html'
    """

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
    cmd = command.split(" ")
    process = subprocess.Popen(cmd, shell=False, stdout=subprocess.PIPE)
    print("Success")

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
    Based on our RST formatting, a module or section title uses a
    particular separator on on the line before and after the title.
    If the "lines" parameter follows this format, we return a tuple
    of a flag indicating if we've found a module or a section, and
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
    Generate a header for the text. We use headers instead of lists
    because they translate to "Heading #" styles in Word, which we
    can pre-format in the reference document.
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


def show_output(output_filename):
    """
    On Windows, we ar going to display the filename and then open the file.
    On Linux, there is no easy automatic way to open the file, so we will
    just show the filename.
    """

    print("Created file: " + output_filename)
    if platform.system() != "Linux":
        print("Opening...")
        os.system("start " + output_filename)


def generate_docx(modules, rst_filename, title):
    """
    Generate an RST file and a DOCX file from the list of modules.
    User may want to edit the resulting DOCX file to make it look
    nicer.
    """

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

    show_output(output_filename)


def generate_html(modules, rst_filename, left):
    """
    Generate an RST file and a HTML file from the list of modules.
    Note the HTML file is just a sanity check. The RST file should
    be the one added to the "public-training-website" repo.
    """

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
    output_filename = run_pandoc("html", rst_filename)
    show_output(output_filename)


def load_one_module(module_filename, short):
    """
    Read the module (recursively to handle "include::" comments).
    Return a tuple of the module title and a list of chapters.
    (List of chapters will be empty if "short" is True).
    """

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

    course_folder = os.path.dirname(os.path.abspath(course))

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
                all_modules.append(
                    load_one_module(
                        os.path.abspath(os.path.join(course_folder, f)), short
                    )
                )

    return all_modules


if __name__ == "__main__":

    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--course",
        required=True,
        help="filename containing list of RST files in the current " + "directory",
    )

    parser.add_argument(
        "--rst",
        required=True,
        help="name of the RST file that will contain " + "the generated RST content",
    )

    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "--html",
        action="store_true",
        help="RST file will be formatted to describe the course "
        + "schedule. This file should be added to the "
        + "'public-training-website' repository in the "
        + "appropriate location.",
    )
    group.add_argument(
        "--docx",
        action="store_true",
        help=(
            "RST file will be formatted for conversion to a Word "
            + "document (used for generating a course syllabus). "
            + "A '.docx' file will also be created - you will "
            + "probably need to clean it up before sending to the "
            + "customer."
        ),
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
