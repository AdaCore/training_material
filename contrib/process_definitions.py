"""
This script is looking for the definition role (:dfn:) in
RST files, so that it can add these definitions to the
list of things that this module provides (and therefore
will appear in the "PROVIDES" section of the module prefix).

This script assumes that the modules are "chapterized" -
meaning that the module only has the module prefix and
"include" directives - no definitions should be in this
file.  It will parse the "include" directives in 
the module file looking for definitions in the chapter
files - but this is not recursive (it will not search
for "include" directives in the chapter files).
(Note that "include" directives should be using "/"
as a path separator).
"""

import argparse
import os

######################
## DEBUG PROCESSING ##
######################

GLOBAL_all_definitions = []


def debug_all_definitions(chapter, definitions):

    global GLOBAL_all_definitions

    for one in definitions:
        GLOBAL_all_definitions.append(one + " => " + chapter)


def debug_print_by_chapter(chapter_definitions):
    print("** Chapter Definitions **")
    for chapter in sorted(chapter_definitions.keys()):
        if len(chapter_definitions[chapter]) > 0:
            print("   * " + chapter + " *")
            for one in sorted(chapter_definitions[chapter]):
                print("      " + one)


def debug_print_by_module(module_name, definitions):
    if len(definitions) > 0:
        print("   * " + module_name + " *")
        for one in sorted(definitions):
            print("      " + one)


def debug_print_all_definitions():
    global GLOBAL_all_definitions

    print("** All Definitions **")
    for one in sorted(GLOBAL_all_definitions):
        print("   " + one)


##########################
## END DEBUG PROCESSING ##
##########################


def is_rst(filename):
    """
    Shortcut to determine if "filename" is a RST file
    """

    return os.path.splitext(filename)[1].lower() == ".rst"


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


def find_chapter_files(module):
    """
    Return a list of files in "include" directives for the module
    """

    chapter_files = []
    with open(module, "r") as file:
        for line in file:
            if line.startswith(".. include::"):
                included = line[13 : len(line)].strip()
                if is_rst(included):
                    chapter_files.append(included)

    return chapter_files


def find_all_definitions(chapter_files):
    """
    Build a dictionary where the key is each element in chapter_files
    and the content is the list of definitions in that file
    """

    all_definitions = {}
    for chapter in sorted(chapter_files):
        all_definitions[chapter] = find_definitions(chapter)
        debug_all_definitions(chapter, all_definitions[chapter])

    return all_definitions


def update_one_module(module_name, chapters, chapter_definitions):
    """
    Search for definitions found in any of the chapters for this
    module. If found, update the "PROVIDES" section of the module
    prefix with the definitions.
    """

    definitions = []
    for chapter in chapters:
        for definition in chapter_definitions[chapter]:
            if not (definition in definitions):
                definitions.append(definition)
    definitions = sorted(definitions)

    """
    TBD - actual updating of the module file
    """

    debug_print_by_module(module_name, definitions)


if __name__ == "__main__":

    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--directory",
        default=".",
        help="directory containing top-level modules (default =" + os.getcwd() + ")",
    )

    args = parser.parse_args()

    module_files = {}
    chapter_files = []

    if not os.path.isdir(args.directory):
        print(args.directory + " does not exist")
    else:
        for file in os.listdir(args.directory):
            if is_rst(file):
                module = os.path.join(args.directory, file)
                module_files[module] = find_chapter_files(module)
                for file in module_files[module]:
                    if not (file in chapter_files):
                        chapter_files.append(file)

    chapter_definitions = find_all_definitions(chapter_files)

    ### DEBUG ###
    debug_print_by_chapter(chapter_definitions)

    debug_print_all_definitions()

    print("** All Modules **")
    ### END DEBUG ###

    for module in module_files.keys():
        update_one_module(module, module_files[module], chapter_definitions)
