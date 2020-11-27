#! /usr/bin/env python3
import sys
from pathlib import Path
import subprocess
import argparse
import re

CURRENT_DIR = Path(sys.argv[0]).parent.resolve()

PANDOC_OPTIONS_HELP_TEXT="""
--from=FORMAT, --read=FORMAT
--to=FORMAT, --write=FORMAT
--output=FILE
--data-dir=DIRECTORY
--base-header-level=NUMBER
--strip-empty-paragraphs
--indented-code-classes=STRING
--filter=PROGRAM
--lua-filter=SCRIPTPATH
--preserve-tabs
--tab-stop=NUMBER
--track-changes=accept|reject|all
--file-scope
--extract-media=PATH
--standalone
--template=FILE
--metadata=KEY[:VALUE]
--metadata-file=FILE
--variable=KEY[:VALUE]
--print-default-template=FORMAT
--print-default-data-file=FILE
--print-highlight-style=STYLE|FILE
--dpi=NUMBER
--eol=crlf|lf|native
--wrap=auto|none|preserve
--columns=NUMBER
--strip-comments
--toc, --table-of-contents
--toc-depth=NUMBER
--no-highlight
--highlight-style=STYLE|FILE
--syntax-definition=FILE
--include-in-header=FILE
--include-before-body=FILE
--include-after-body=FILE
--resource-path=SEARCHPATH
--request-header=NAME:VALUE
--self-contained
--html-q-tags
--ascii
--reference-links
--reference-location=block|section|document
--atx-headers
--top-level-division=section|chapter|part
--number-sections
--number-offset=NUMBERS
--listings
--incremental
--slide-level=NUMBER
--section-divs
--default-image-extension=extension
--email-obfuscation=none|javascript|references
--id-prefix=STRING
--title-prefix=STRING
--css=URL
--reference-doc=FILE
--epub-subdirectory=DIRNAME
--epub-cover-image=FILE
--epub-metadata=FILE
--epub-embed-font=FILE
--epub-chapter-level=NUMBER
--pdf-engine=PROGRAM
--pdf-engine-opt=STRING
--bibliography=FILE
--csl=FILE
--citation-abbreviations=FILE
--natbib
--biblatex
--mathml
--webtex[=URL]
--mathjax[=URL]
--katex[=URL]
--gladtex
--abbreviations=FILE
--trace
--dump-args
--ignore-args
--verbose
--quiet
--fail-if-warnings
--log=FILE
--bash-completion
--list-input-formats
--list-output-formats
--list-extensions[=FORMAT]
--list-highlight-languages
--list-highlight-styles
--version
--help
  """
PANDOC_OPTION_RE = re.compile("\-\-([^\s,\[=]+)")
PANDOC_OPTIONS = PANDOC_OPTION_RE.findall(PANDOC_OPTIONS_HELP_TEXT)

def parse_defaults(f):
    with open(f) as ff:
        data = ff.read()

    setup = ""
    # Ugly parsing for a very simple YAML
    # This is necessary because the --defaults pandoc option 
    # is not available on current Debian repos...
    # https://pandoc.org/MANUAL.html#default-files for format
    for l in data.split('\n'):
        l = l.strip()
        kv = l.split(': ')

        if len(kv) < 2:
            continue
        k, v = kv[0], ": ".join(kv[1:])
        if k in PANDOC_OPTIONS:
            if v.strip().lower() in ("true", "yes"):
                setup += f"--{k} "
            else:
                setup += f"--{k}={v} "
        else:
            setup += f"-V '{k}={v}' "

    return setup

if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--config", default=CURRENT_DIR / 'default_lab_docs.yaml' )
    ap.add_argument("input")
    ap.add_argument("output")
    ap.add_argument("args", nargs="*")
    args = ap.parse_args()

    default_args = parse_defaults(args.config)
    subprocess.check_call(f"pandoc " + \
                          f"--data-dir={(CURRENT_DIR.parent / 'support_files').resolve()} " + \
                          f"{Path(args.input).resolve()} -o {Path(args.output).resolve()} " + \
                          f"{default_args} {' '.join(args.args)}",
                          cwd=CURRENT_DIR.parent,
                          shell=True)
