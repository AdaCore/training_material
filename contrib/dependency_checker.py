import argparse
import os

"""
Map where the key is the module name, and the content
is a list of concepts the module provides.
"""
GLOBAL_map_module_to_description = {}

"""
Map where the key is a concept description, and the
content is the module(s) where the concept is provided.
"""
GLOBAL_map_description_to_module = {}


def process_one_file(module_filename):
    """
    Read a module and store the concepts listed in
    the 'Provides' section.
    """

    global GLOBAL_map_module_to_description
    global GLOBAL_map_description_to_module

    with open(module_filename, "r") as f:
        lines = f.readlines()

    basename = os.path.splitext(module_filename)[0]

    start_processing = False

    for line in lines:
        stripped = line.strip()
        if len(stripped) == 0:
            pass
        elif "PRELUDE PROVIDES" in stripped:
            start_processing = True

        elif start_processing:

            if "container::" in line:
                break

            elif not stripped[0:1].isalpha():
                pass

            else:
                provides = stripped
                if basename not in GLOBAL_map_module_to_description:
                    GLOBAL_map_module_to_description[basename] = []
                if provides not in GLOBAL_map_description_to_module:
                    GLOBAL_map_description_to_module[provides] = []
                GLOBAL_map_module_to_description[basename].append(provides)
                GLOBAL_map_description_to_module[provides].append(basename)


def process_data():
    """
    At the moment, this is just a debugging process that prints
    out the contents of the maps.
    """

    global GLOBAL_map_module_to_description
    global GLOBAL_map_description_to_module

    for module in sorted(GLOBAL_map_module_to_description.keys()):
        print(module)
        for description in GLOBAL_map_module_to_description[module]:
            print("   " + description)

    for description in sorted(GLOBAL_map_description_to_module.keys()):
        print(description)
        for module in GLOBAL_map_description_to_module[description]:
            print("   " + module)


if __name__ == "__main__":

    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--course", help="file containing structure of course", required=True
    )

    args = parser.parse_args()

    with open(args.course) as fp:
        for line in fp:
            if os.path.exists(line.strip()):
                process_one_file(line.strip())

    process_data()
