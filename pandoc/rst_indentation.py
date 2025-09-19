import argparse
import os
import re


def fix_codeblock_indentation(lines):
    fixed_lines = []
    list_indent = None
    new_code_indent = None
    original_code_indent = None

    # Match *, -, or + bullets
    bullet_re = re.compile(r"^(\s*)([*\-+])\s+")

    for line in lines:

        # Reset everything when we see a slide separator
        if line.startswith("---"):
            fixed_lines.append(line)
            list_indent = None
            new_code_indent = None
            original_code_indent = None
            continue

        # Set the list indentation when we see a container
        # (but we know we're not in a code block, so reset those flags)
        if line.strip().startswith(".. container"):
            list_indent = len(line) - len(line.lstrip())
            new_code_indent = None
            original_code_indent = None
            fixed_lines.append(line)
            continue

        # Always echo blank lines
        if len(line.strip()) == 0:
            fixed_lines.append(line)
            continue

        # If we're in a code block, just echo anything
        # that is part of the code block
        if new_code_indent != None:
            this_indent = len(line) - len(line.lstrip())

            if this_indent <= original_code_indent:
                # This is outside of the code block.
                # If we've shifted the code block left, we want to
                # make sure we shift this left the same amount
                if new_code_indent < original_code_indent:
                    shift = original_code_indent - new_code_indent
                    shift = max(this_indent - shift, 0)
                    new_indent = shift * " "
                    line = new_indent + line.lstrip()
                    new_code_indent = None
                    original_code_indent = None
            else:
                # This is inside the code block.
                # We need to shift everything the same amount that
                # we shifted the code directive.
                shift = (new_code_indent - original_code_indent) + this_indent
                line = (shift * " ") + line.lstrip()

            fixed_lines.append(line)
            continue

        # If we find a bullet, set the list indentation and reset everything else
        m = bullet_re.match(line)
        if m:
            list_indent = len(m.group(1))
            new_code_indent = None
            original_code_indent = None
            fixed_lines.append(line)
            continue

        # Detect code directive
        if line.lstrip().startswith(".. code::"):

            if line.startswith(".. code::"):
                # If the directive starts in the first column, we don't
                # want to indent it under some list item.
                new_code_indent = 0
                original_code_indent = 0

            elif list_indent is not None:
                # indent two spaces after bullet
                new_code_indent = list_indent + 2
                original_code_indent = len(line) - len(line.lstrip())
                spaces = new_code_indent * " "
                line = spaces + line.lstrip()
            fixed_lines.append(line)
            continue

        # All other lines
        fixed_lines.append(line)

    return fixed_lines


def process_file(filename):

    if filename.lower().endswith(".rst"):

        lines = None
        with open(filename, "r") as f:
            lines = f.read()

        lines = lines.split("\n")
        fixed_lines = fix_codeblock_indentation(lines)

        with open(filename, "w") as f:
            for line in fixed_lines:
                f.write(line + "\n")


if __name__ == "__main__":

    parser = argparse.ArgumentParser()

    group = parser.add_mutually_exclusive_group(required=True)

    group.add_argument("--filename", help="name of file to fix")

    group.add_argument(
        "--directory", help="directory containing files to fix (recursive)"
    )

    args = parser.parse_args()

    if args.filename is not None:
        if not os.path.isfile(args.filename):
            print(args.filename + " does not exist")
        else:
            process_file(args.filename)

    else:
        if not os.path.isdir(args.directory):
            print(args.directory + " does not exist")
        else:
            for root, dirs, files in os.walk(args.directory):
                for file in files:
                    process_file(os.path.join(root, file))
