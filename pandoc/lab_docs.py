#! /usr/bin/env python3
import sys
from pathlib import Path
import subprocess
import argparse
import parse_pandoc_defaults

CURRENT_DIR = Path(sys.argv[0]).parent.resolve()

def lab_doc(input, output, default_args, args):
    print(f"{input} -> {output}")
    subprocess.check_call(f"pandoc " + \
                          f"--data-dir={(CURRENT_DIR.parent / 'support_files').resolve()} " + \
                          f"{input.resolve()} -o {output.resolve()} " + \
                          f"{default_args} {' '.join(args)}",
                          cwd=CURRENT_DIR.parent,
                          shell=True)

if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--defaults", default=CURRENT_DIR / 'default_lab_docs.yaml' )
    ap.add_argument("--recursive", "-r", action="store_true",
                    help="Take the input as a base dir and generate from all directories there "
                         "that contain a file named INDEX_FILE_NAME (see --index-file-name)")
    ap.add_argument("--index-file-name", default="instructions.rst")
    ap.add_argument("input", type=Path)
    ap.add_argument("output", type=Path)
    ap.add_argument("args", nargs="*")
    args = ap.parse_args()

    if args.recursive:
        args.output.mkdir(parents=True, exist_ok=True)
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)

    default_args = parse_pandoc_defaults.parse_defaults(args.defaults)

    if args.recursive:
        for f in args.input.glob("*/" + args.index_file_name):
            lab_doc(f, args.output / f"{f.parent.name}.pdf", default_args, args.args)
    else:
        lab_doc(args.input, args.output, default_args, args.args)
