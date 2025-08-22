"""
This script is designed to create a lab folder based on the text
file used to create slides from the material/courses repository
(e.g. "standard_course.txt").

For every module listed in the TXT file, the appropriate folder
will be used to populate a "labs" folder. Each module will have
a "prompt" and "answer" folder. If the "ada95" switch is set,
then, if there is an "ada95" folder in the repository, it will
replace the "answer" folder in the destination.

Switches:
   --course      => filename containing list of RST files in the current
                    directory
                    (full path can be specified)
   --destination => folder that will contain the "labs" folder
                    (this allows you to have a folder with the
                    PDF and anything else already set up)
   --ada95       => If set, the answers will be the "Ada95" version
                    of the answers
"""

import argparse
import copy
import os
import shutil


def safe_copy(source, destination):

    if os.path.exists(source):
        shutil.copytree(source, destination)
        return True
    else:
        return False


def build_folder(course, destination, ada95):

    directory = os.path.dirname(course)

    with open(course) as f:
        filenames = f.read().splitlines()

        for f in filenames:
            if f.startswith("--"):
                pass
            elif f.startswith("#"):
                pass
            else:
                pieces = f.split(".")[0]
                pieces = pieces.split("-")
                module = pieces[0]
                special = ""
                if len(pieces) > 1:
                    special = "-" + pieces[1]

                source_folder = os.path.join(directory, (os.path.join(module, "lab")))
                source_folder = os.path.join(source_folder, module[4:] + special)

                destination_folder = os.path.join(destination, "labs")
                destination_folder = os.path.join(destination_folder, module[4:])
                os.makedirs(destination_folder, exist_ok=True)

                if safe_copy(
                    os.path.join(source_folder, "prompt"),
                    os.path.join(destination_folder, "prompt"),
                ):

                    answer_copied = False
                    if ada95:
                        answer_copied = safe_copy(
                            os.path.join(source_folder, "ada95"),
                            os.path.join(destination_folder, "answer"),
                        )
                    if not answer_copied:
                        answer_copied = safe_copy(
                            os.path.join(source_folder, "answer"),
                            os.path.join(destination_folder, "answer"),
                        )
                    if not answer_copied:
                        print("No answer folder for " + module[4:])

                else:
                    print("No prompt folder for " + module[4:])


if __name__ == "__main__":

    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--course",
        required=True,
        help="filename containing list of RST files (full path allowed)",
    )

    parser.add_argument(
        "--destination",
        required=True,
        help='directory where to create the "labs" folder',
    )

    parser.add_argument(
        "--ada95", action="store_true", help="set if class is focusing on Ada95"
    )

    args = parser.parse_args()

    course = os.path.abspath(args.course)
    if not os.path.isfile(course):
        print('"' + args.course + '" does not specify an existing file')
    else:
        build_folder(course, os.path.abspath(args.destination), args.ada95)
