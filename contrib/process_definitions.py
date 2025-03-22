import argparse
import os

GLOBAL_filenames = []


def is_rst(filename):
    """
    Shortcut to determine if "filename" is a RST file
    """

    return os.path.splitext(filename)[1].lower() == ".rst"


def add_filename_to_list(filename):
    """
    Make sure "filename" is in a platform-dependent state, then
    add it to the list of files we care about
    """

    global GLOBAL_filenames

    partial = filename.replace("\\", "/")
    pieces = filename.split("/")
    to_append = os.path.join(*pieces)
    if to_append not in GLOBAL_filenames:
        GLOBAL_filenames.append(to_append)


def find_definitions(filename):
    """
    Read each line of "filename" searching for ":dfn:".
    If found, add it to the list of definitions found for this file
    """

    retval = []

    with open(filename, "r") as file:
        contents = file.read()

        start = contents.find(":dfn:")
        while start >= 0:
            start = contents.find("`", start)
            if start < 0:
                break
            end = contents.find("`", start + 1)
            if end < 0:
                break
            retval.append(contents[start + 1 : end].lower())
            start = contents.find(":dfn:", start)

    return retval


def find_included_files(filename):
    """
    Search 'filename' for an inclusion of an RST file.
    If found, add it to the list of files we care about
    """

    with open(filename, "r") as file:
        for line in file:
            if line.startswith(".. include::"):
                included = line[13 : len(line)].strip()
                if is_rst(included):
                    add_filename_to_list(included)


def build_filenames_list(filename):
    """
    Add 'filename' and all the files it includes to the
    list of files we care about
    """

    if filename not in GLOBAL_filenames:
        add_filename_to_list(filename)
        find_included_files(filename)


def find_all_definitions():
    """
    For each file we care about, print out the
    definitions we find.
    """

    global GLOBAL_filenames

    output = []
    for filename in sorted(GLOBAL_filenames):
        definitions = find_definitions(filename)
        for one in definitions:
            output.append(one + " => " + filename)

    for d in sorted(output):
        print(d)


if __name__ == "__main__":

    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--directory",
        default=".",
        help="directory containing top-level modules (default =" + os.getcwd() + ")",
    )

    args = parser.parse_args()

    if not os.path.isdir(args.directory):
        print(args.directory + " does not exist")
    else:
        for file in os.listdir(args.directory):
            if is_rst(file):
                build_filenames_list(os.path.join(args.directory, file))

    find_all_definitions()
