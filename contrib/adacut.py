#!/usr/bin/env python3
import argparse
import re
import sys


class AdaCut:
    RE_DIRECTIVE = re.compile(r"^\s*\-\-\$\s+(\S+)\s+(\S+)(\s+(\S+))?\s*$")
    RE_DIRECTIVE_PARTIAL = re.compile(r"^\s*\-\-\$.*$")

    DIRECTIVES = ["begin", "end", "line"]
    TYPES = ["answer", "question", "cut"]
    TARGETS = ["code", "comment", "all"]

    RE_PURE_COMMENT = re.compile(r"^\s*\-\-.*$")

    def __init__(self, cut, mode, default_keeping=True):
        self.cut = cut
        self.current_cut = 0
        if self.cut:
            # Cut mode: throw everything but the given cut
            self.mode = None
            self.default_keeping = False
        else:
            self.mode = mode
            self.default_keeping = default_keeping
        self.line = None
        self.block = []

    def match_mode_or_cut(self, typ_name):
        if self.cut:
            return self.current_cut == self.cut
        else:
            return self.mode == typ_name or self.mode == "keep_all"

    def keeping_code_comments_with(self, typ):
        if self.match_mode_or_cut(typ[0]):
            return True, True
        elif typ[1] == "comment":
            return True, False
        elif typ[1] == "code":
            return False, True
        else:
            return False, False

    def keeping_code_comments(self):
        if self.line:
            return self.keeping_code_comments_with(self.line)
        elif self.block:
            return self.keeping_code_comments_with(self.block[-1])
        else:
            return self.default_keeping, self.default_keeping

    def new_line(self, l):
        keeping_code, keeping_comments = self.keeping_code_comments()
        self.line = None

        is_code = not self.RE_PURE_COMMENT.match(l)
        if is_code:
            if keeping_code:
                return l
            elif keeping_comments:
                # Special case: Keep empty lines to fill
                return l[-1]
            else:
                None

        m = self.RE_DIRECTIVE.match(l)
        if not m:
            assert not self.RE_DIRECTIVE_PARTIAL.match(
                l
            ), f"malformed --$ comment: {l[:-1]}"
            return l if keeping_comments else None

        directive = m.group(1).lower()
        typ = m.group(2).lower(), m.group(4).lower() if m.group(4) else "all"
        if directive not in self.DIRECTIVES:
            # Warning
            print("unknown directive:", directive, file=sys.stderr)
            return
        if typ[0] not in self.TYPES:
            # warning
            print("unknown type:", typ[0], file=sys.stderr)
            return
        if typ[1] not in self.TARGETS:
            # warning
            print("unknown target:", typ[1], file=sys.stderr)
            return

        if directive == "begin":
            self.block.append(typ)
            if typ[0] == "cut":
                self.current_cut += 1
        elif directive == "end":
            assert typ == self.block[-1], f"{typ} != {self.block[-1]}"
            self.block = self.block[:-1]
        elif directive == "line":
            self.line = typ
        else:
            assert False, "Bug!"


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("input_file")
    ap.add_argument("-o", "--output-file")
    ap.add_argument("-c", "--cut", type=int)
    ap.add_argument("-d", "--dedent", action="store_true",
                    help="Dedent by the first-line indent")
    ap.add_argument("-C", "--cut-counting", action="store_true",
                    help="Return the number of cuts in the file")
    ap.add_argument(
        "-m", "--mode", default="question",
        choices=["answer", "question", "keep_all"]
    )
    args = ap.parse_args()

    if args.output_file:
        out = open(ap, "w")
    else:
        out = sys.stdout

    cut = AdaCut(args.cut, args.mode)
    dedent_cols = None
    with open(args.input_file) as fin:
        for l in fin:
            lp = cut.new_line(l)
            if lp != None:
                if args.dedent:
                    if dedent_cols is None:
                        dedent_cols = len(lp) - len(lp.lstrip())

                    if lp.strip() != '':
                        assert lp[:dedent_cols] == ' ' * dedent_cols, repr(lp)
                        lp = lp[dedent_cols:]

                print(lp, file=out, end="")

    if args.cut_counting:
        print(cut.current_cut)
