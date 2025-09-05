import argparse
import os
import sys
import subprocess
import tempfile
import multiprocessing
from pathlib import Path

"""
Keys for content information in title page file.
If no key is specified on a line in the file,
the first item in this list will be used as the key.
Uses:
   COPYRIGHT - any line that does not start with a key will
       be stored in the dictionary using this key
   TITLE - Title of document
   PRESENTER - Will be shown as "Presented by <value>"
       right below the title
   OTHER - if you want something else. Will appear after
       PRESENTER
   DATE - Usually the date. Appears right below OTHER
"""
TITLE_KEYS = ["COPYRIGHT", "TITLE", "PRESENTER", "OTHER", "DATE"]

"""
if TITLE_FILE_NAME is set, we are creating a deck that needs
a title page. This object will have the name of the temporary
file that contains the title page information, and that file
will be deleted when the process is complete.
"""
TITLE_FILE_NAME = None


def windows():
    """
    Return True if we're running on a Windows platform
    """

    return sys.platform.startswith("win")


def fix_file_case(filename):
    """
    When building a list of directories to search, we want them to be unique
    (so we don't end up searching directories twice). For Windows, case
    doesn't matter, so we are going to force it to lower case
    """

    if windows():
        return filename.lower()
    return filename


def title_parse(line):
    """
    Parse a line that was read from the title page data file.
    If the line starts with a keyword, a tuple of keyword and
    content is returned. Otherwise, a tuple of "COPYRIGHT" and
    content is returned.
    """

    key = "COPYRIGHT"
    pieces = line.split(":", 1)
    if pieces[0] in TITLE_KEYS:
        return pieces[0], pieces[1].strip()
    else:
        return TITLE_KEYS[0], line.strip()


def default_title(source):
    """
    If no title is specified, use the folder name to determine title
    """

    title = os.path.basename(os.path.dirname(os.path.abspath(source)))
    if title.lower() == "gnatdas":
        return "GNAT DAS"
    elif title.lower() == "gnatsas":
        return "GNAT SAS"
    pieces = title.split("_")
    title = ""
    for piece in pieces:
        title = title + piece[0].upper() + piece[1:] + " "
    return title.strip()


def parse_title_file(args):
    """
    Read the title data file to generate information for the title page.
    If the data file is not passed in as an argument, use the file
    that exists in the same directory.
    Note that "COPYRIGHT" returns a list of lines; all other keywords
    are just a string.
    """

    title_source = args.title.strip()

    retval = {}
    for key in TITLE_KEYS:
        if key == "COPYRIGHT":
            retval[key] = []
        else:
            retval[key] = ""

    try:
        fp = None
        if os.path.exists(title_source):
            fp = open(title_source, "r")
        else:
            fp = open(os.path.join(os.path.dirname(__file__), "title_data.txt"), "r")
        for line in fp:
            key, content = title_parse(line.strip())
            if len(content) > 0 or key == "COPYRIGHT":
                if key == "COPYRIGHT":
                    retval[key].append(content)
                else:
                    retval[key] = content

        if len(retval["TITLE"]) == 0:
            retval["TITLE"] = default_title(args.source[0])

    except:
        pass

    return retval


def print_content(tfp, content, prefix=""):
    """
    if content is not empty, it's either a list or a string.
    For lists, print out each line separately, for a string,
    print the prefix in italics and the content in bold.
    """

    if len(content) > 0:
        if content is list:
            for item in content:
                tfp.write(
                    "   \\small{\\textit{" + prefix + "} \\textbf{" + item + "}}\n\n"
                )
        else:
            tfp.write(
                "   \\small{\\textit{" + prefix + "} \\textbf{" + content + "}}\n\n"
            )


def prepare_title_file(args):
    """
    If the "source" command line argument is only one TXT file, then we
    assume we are producing a slide deck (anything else is just trying
    to build from individual RST files).
    In that case, we want to build a title slide for the deck,
    consisting of the title for the course, possibly the presenter, date
    and/or other information, and then copyright information.
    This information will come from the "title_data.txt" file in the
    "pandoc" folder OR from the "--title" option from the command line.
    """

    global TITLE_FILE_NAME

    TITLE_FILE_NAME = None
    if len(args.source) == 1 and args.source[0].lower().endswith(".txt"):
        tmp = tempfile.NamedTemporaryFile(delete=False)
        TITLE_FILE_NAME = tmp.name + ".rst"
        title_info = parse_title_file(args)

        with open(TITLE_FILE_NAME, "w") as tfp:

            """
            This is the actual title page for the beamer slide deck
            """
            separator = "#" * (2 + len(title_info["TITLE"]))
            tfp.write(separator + "\n")
            tfp.write(title_info["TITLE"] + "\n")
            tfp.write(separator + "\n\n")

            """
            This is the copyright page (which will include any other user-specified
            information) that appears after the document title page and before
            the document actually begins.
            """

            tfp.write(".. raw:: latex\n\n")
            tfp.write("   \\begin{center}\n")

            tfp.write("   \\colorbox{adacore3}\n")
            tfp.write(
                "   {\\color{adacore1}{\\huge{\\textit{\\textbf{"
                + title_info["TITLE"]
                + "}}}}}\n\n"
            )

            print_content(tfp, title_info["PRESENTER"], prefix="Presented by")
            print_content(tfp, title_info["OTHER"])
            print_content(tfp, title_info["DATE"])

            tfp.write("   \\end{center}\n\n")

            tfp.write("   \\vspace{3cm}\n\n")

            if len(title_info["COPYRIGHT"]) > 0:
                tfp.write("   \\tiny{\n")
                for line in title_info["COPYRIGHT"]:
                    tfp.write("     " + line + "\n")
                tfp.write("   }\n")


def to_texinputs_path(path):
    """
    Convert path to the correct form for the TEXINPUTS
    environment variable.
    """

    abspath = fix_file_case(os.path.abspath(path))
    if path.endswith("//"):
        # (texlive specific?) recursive path
        abspath += "//"
    return abspath


def extend_texinputs_path_list(unique, paths):
    """
    Add paths to the TEXINPUTS environment variable needed for
    Pandoc/LaTeX
    """

    for path in paths:
        abspath = to_texinputs_path(path)
        if abspath not in unique:
            unique.append(abspath)


def set_texinputs(new_directories):
    """
    Set the TEXINPUTS environment variable based on the directories specified
    on the command line. These directories will contain things like images
    and Beamer theme files
    """

    # initialize list of directories
    unique = list()

    # add user-specified directories to front of list
    extend_texinputs_path_list(unique, new_directories.split(","))

    # add any previously existing directories
    current = os.environ.get("TEXINPUTS", "")

    # default separator (works for Windows and linux 'sh')
    separator = ";"
    # For linux, check if we're using ':' or ';' as a separator
    if not windows():
        # if we don't find a ";" separator, we will use ':'
        if not ";" in current:
            separator = ":"

    # add current TEXINPUTS paths
    extend_texinputs_path_list(unique, current.split(separator))

    # when TEXINPUTS ends w/ a separator it means to append to standard TeX paths
    texinputs_append = len(current) and current[-1] == separator
    texinputs_formated = separator.join(unique) + (
        separator if texinputs_append else ""
    )
    os.environ["TEXINPUTS"] = texinputs_formated
    return texinputs_formated


def output_file_name(
    input_file,
    n,
    extension,
    title=None,
    output_file=None,
    strip_extension=False,
    output_dir=None,
):
    """
    Choose and set up an output file full path based on various arguments received.
    """

    if output_file:
        name = output_file
    elif title:
        name = title
    else:
        name = os.path.basename(input_file)

    # In case of multiple inputs, add number
    if n:
        name += f" {n}"

    if strip_extension:
        name = str(Path(name).with_suffix(f".{extension}"))
    else:
        name += f".{extension}"

    if output_dir:
        output_dir_path = Path(args.output_dir)
        output_dir_path.mkdir(parents=True, exist_ok=True)
        name = str(output_dir_path / name)

    return os.path.abspath(name)


def output_format(extension):
    """
    For PDF and TEX, we are producing slides, so use the 'beamer' format.
    For any other extension, assume the extension and the format are the same.
    """

    if extension == "pdf" or extension == "tex":
        return "beamer"
    elif extension == "md":
        return "markdown"
    else:
        return extension


def parse_rst_list_file(dirname, f):
    """
    Parse a file containing a list of RST entries, empty lines, or comments.

    dirname : str - a path to the target directory
    f : [str] - a list of lines, which make up the rst-list file content

    Returns [str] - the list of resolved files

    Each RST entry is a file path, starting from the given dirname

    The file content can look like ("> " prefix added to each line)

    > # Full course on salad dressing
    > buy_a_lettuce.rst
    > wash_the_salad.rst # Unless you like sand...
    > # Apparently it's not necessary?
    > # cut_the_salad.rst
    > dress_your_salad.rst

    """

    files = list()
    for line in f:
        if "#" in line:
            source = line[: line.index("#")].strip()
        elif "--" in line:
            source = line[: line.index("--")].strip()
        else:
            source = line.strip()

        if source == "":
            continue

        # Generate full path
        path = os.path.abspath(os.path.join(dirname, source))
        assert os.path.isfile(path), path + " does not exist"
        # If there are spaces in the path, enclose path in quotes
        if " " in path:
            path = '"' + path + '"'
        files.append(path)
    return files


def expand_source(source_file):
    """
    If the source file is an RST file, then send it to Pandoc
    For any other type of file, assume it contains a list of
    files to parse
    NOTE: Pandoc treats multiple source files on the command
    line DIFFERENTLY than all the source files combined as one!
    (When multiple source files, each source file is its own section)

    If this is a slide deck for presentation, we want to add a
    title page to it, so just insert the title page RST file to the
    front of the list.
    """

    global TITLE_FILE_NAME

    if source_file.lower().endswith(".rst"):
        return [os.path.abspath(source_file)]
    else:
        dirname = os.path.dirname(source_file)
        # Read lines from source file
        with open(source_file) as f:
            files = parse_rst_list_file(dirname, f)
        if TITLE_FILE_NAME != None:
            files.insert(0, TITLE_FILE_NAME)
        return files


def pandoc_prepare_run(args):
    return (
        pandoc_prepare_run_single(n, source_or_source_list, args)
        for n, source_or_source_list in enumerate(args.source)
    )


def pandoc_prepare_run_single(n, source_or_source_list, args):
    assert os.path.isfile(source_or_source_list), (
        source_or_source_list + " does not exist"
    )

    theme = args.theme
    if len(theme) > 0:
        theme = " -V theme=" + theme

    color = args.color
    if len(color) > 0:
        color = " -V colortheme=" + color

    input_file = source_or_source_list

    extension = args.extension
    if extension is None:
        extension = "pdf"  # Default
        if args.output is not None:
            spl = args.output.rsplit(".", 1)
            if spl:
                extension = spl[-1]

    # Output default value is input file name
    output_file = output_file_name(
        input_file,
        n if len(args.source) > 1 else None,
        extension,
        title=args.title,
        output_dir=args.output_dir,
        output_file=args.output,
        strip_extension=not args.do_not_strip_extension,
    )
    filter = args.filter
    if not os.path.isfile(filter):
        filter = os.path.join(os.path.dirname(__file__), filter)
        if not os.path.isfile(filter):
            filter = ""
    if os.path.isfile(filter):
        filter = " --filter " + filter

    syntax = "--syntax-definition=" + os.path.join(os.path.dirname(__file__), "bnf.xml")

    # build list of search directories
    texinputs = set_texinputs(args.directories)
    if not args.hush:
        print(f"TEXINPUTS={os.environ['TEXINPUTS']}")

    source_list = expand_source(source_or_source_list)

    assert len(source_list) != 0, "No source files found"

    command = (
        "pandoc --standalone",
        "--resource-path",
        texinputs,
        filter,
        syntax,
        theme,
        color,
        "--fail-if-warnings",
        "-f rst",
        "-t " + output_format(extension.lower()),
        "-o " + output_file,
        *source_list,
    )

    if not args.hush:
        print(" ".join(command))

    return {
        "cwd": os.path.dirname(source_or_source_list),
        "env": {k: str(v) for k, v in os.environ.items()},
        "source": source_or_source_list,
        "input_file": input_file,
        "output_file": output_file,
        "command": command,
    }


def run_pandoc(args):
    task_name = f"{args['input_file']} -> {args['output_file']}"

    print(f"[start] {task_name}")
    try:
        subprocess.run(
            " ".join(args["command"]),
            cwd=args["cwd"],
            env=args["env"],
            shell=True,
            check=True,
        )
    except Exception as e:
        print(f"\033[1;31m[error]\033[0m {task_name}", file=sys.stderr)
        print(e, file=sys.stderr)
        print()
        raise
    print(f"[end  ] {task_name}")


def delete_temp_file():
    global TITLE_FILE_NAME
    if TITLE_FILE_NAME != None and os.path.exists(TITLE_FILE_NAME):
        os.remove(TITLE_FILE_NAME)
        # print (TITLE_FILE_NAME)


if __name__ == "__main__":
    PANDOC = Path(sys.argv[0]).resolve().parent
    ROOT = PANDOC.parent

    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--source",
        help="Source RST file OR text file with list of RST files",
        nargs="+",
        required=True,
    )

    parser.add_argument("--output", help="Output file name (without extension)")

    parser.add_argument(
        "--extension",
        help='Output file extension. "PDF" => Beamer, "TEX" => LaTeX, "DOCX" => Word, "PPTX" => PowerPoint',
        default=None,
        required=False,
    )

    parser.add_argument(
        "--directories",
        help="Comma-separated list of folders to search for things like images and Beamer themes",
        default=f"{ROOT}//,:{ROOT}/images:",
        required=False,
    )
    parser.add_argument("-j", "--jobs", type=int, default=0, required=False)

    parser.add_argument(
        "--title",
        help="Document title and name of output file. "
        "If not specified, output file will be source filename "
        "with specified extension.\n"
        "If several sources are specified, each one will have a "
        "number appended after its title.",
        default="",
        required=False,
    )

    parser.add_argument(
        "--theme", help="Beamer theme", default="adacore", required=False
    )

    parser.add_argument(
        "--color", help="Beamer color theme", default="adacore", required=False
    )

    parser.add_argument(
        "--filter",
        help="Pandoc filter to do special processing",
        default=str(PANDOC / "beamer_filter.py"),
        required=False,
    )

    parser.add_argument("--output-dir", help="Output directory", required=False)

    parser.add_argument(
        "--do-not-strip-extension",
        help="Do not strip the original extension from the title of "
        "the output file.\nLegacy behaviour\n"
        "Eg. 'foo.rst.pdf' will become 'foo.pdf'.",
        action="store_true",
    )

    parser.add_argument("--hush", help="Hide ran commands.", action="store_true")

    args = parser.parse_args()

    prepare_title_file(args)

    pandoc_prepared = pandoc_prepare_run(args)

    try:
        with multiprocessing.Pool(None if args.jobs == 0 else args.jobs) as p:
            p.map(run_pandoc, pandoc_prepared)
        delete_temp_file()
    except subprocess.CalledProcessError as e:
        delete_temp_file()
        sys.exit(2)
