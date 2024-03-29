#!/usr/bin/env python3
import argparse
import re
import sys
import enum


class CutState(enum.Enum):
    NONE = 0
    IN_BLOCK = 1
    IN_LINE = 2


class AdaCut:
    RE_DIRECTIVE = re.compile(r"^\s*\-\-\$\s+(\S+)\s+(\S+)(\s+(\S+))?\s*$")
    RE_DIRECTIVE_PARTIAL = re.compile(r"^\s*\-\-\$.*$")

    DIRECTIVES = ["begin", "end", "line"]
    TYPES = ["answer", "question", "cut"]
    TARGETS = ["code", "comment", "all"]

    RE_PURE_COMMENT = re.compile(r"^\s*\-\-.*$")

    def __init__(self, cut, mode, default_keeping):
        self.lines = 0
        self.directives = 0
        self.comments = 0
        self.kept = 0

        self.default_keeping = default_keeping
        self.cut = cut
        self.cut_state = CutState.NONE
        self.current_cut = 0
        if self.cut and not self.default_keeping:
            # Pure cut mode: throw everything but the given cut(s)
            self.mode = None
        else:
            self.mode = mode
        self.line = None
        self.block = []

    def match_mode_or_cut(self, typ_name):
        keep = False
        if self.cut and self.cut_state != CutState.NONE:
            keep = self.current_cut in self.cut
        if (not self.cut) or self.default_keeping:
            keep = keep or self.mode == typ_name or self.mode == "keep_all"
        return keep

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
        if self.cut_state == CutState.IN_LINE:
            self.cut_state = CutState.NONE
        self.lines += 1

        is_code = not self.RE_PURE_COMMENT.match(l)
        if is_code:
            if keeping_code:
                self.kept += 1
                return l
            elif keeping_comments:
                self.kept += 1
                # Special case: Keep empty lines to fill
                return l[-1]
            else:
                None

        m = self.RE_DIRECTIVE.match(l)
        if not m:
            assert not self.RE_DIRECTIVE_PARTIAL.match(
                l
            ), f"malformed --$ comment: {l[:-1]}"

            self.comments += 1
            if keeping_comments:
                self.kept += 1
                return l
            else:
                return None

        self.directives += 1
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
                self.cut_state = CutState.IN_BLOCK
                self.current_cut += 1
        elif directive == "end":
            assert typ == self.block[-1], f"{typ} != {self.block[-1]}"
            self.block = self.block[:-1]
            if typ[0] == "cut":
                self.cut_state = CutState.NONE
        elif directive == "line":
            self.line = typ
            if typ[0] == "cut":
                self.cut_state = CutState.IN_LINE
                self.current_cut += 1
        else:
            assert False, "Bug!"


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("input_file")
    ap.add_argument("-o", "--output-file")
    ap.add_argument("-c", "--cut", nargs="*", type=int)
    ap.add_argument(
        "-k",
        "--default-keeping",
        action="store_true",
        help="Set for answer, question, keep_all modes by default",
    )
    ap.add_argument(
        "-K",
        "--no-default-keeping",
        action="store_true",
        help="Set for cut mode by default",
    )
    ap.add_argument(
        "-d", "--dedent", action="store_true", help="Dedent by the first-line indent"
    )
    ap.add_argument(
        "-C",
        "--cut-counting",
        action="store_true",
        help="Return the number of cuts in the file",
    )
    ap.add_argument(
        "-m", "--mode", default="question", choices=["answer", "question", "keep_all"]
    )
    args = ap.parse_args()

    if args.output_file:
        out = open(ap, "w")
    else:
        out = sys.stdout

    output_cut = not args.cut_counting
    assert not (args.default_keeping and args.no_default_keeping)
    if args.default_keeping:
        default_keeping = True
    elif args.no_default_keeping:
        default_keeping = False
    else:
        default_keeping = not args.cut

    cut = AdaCut(args.cut, args.mode, default_keeping=default_keeping)
    dedent_cols = None
    prev_cut = None
    prev_ln = 0
    prev_indent = None
    with open(args.input_file) as fin:
        for l in fin:
            lp = cut.new_line(l)
            if lp != None:
                cur_ln = cut.lines - cut.directives
                cur_indent = len(lp) - len(lp.lstrip())
                if args.dedent:
                    if dedent_cols is None:
                        dedent_cols = cur_indent

                    if lp.strip() != "":
                        assert lp[:dedent_cols] == " " * dedent_cols, repr(lp)
                        lp = lp[dedent_cols:]

                if (
                    args.cut
                    and not default_keeping
                    and prev_cut
                    and prev_cut != cut.current_cut
                    and prev_ln != cur_ln - 1
                ):
                    print((" " * max(cur_indent, prev_indent)) + "...", file=out)
                prev_ln = cur_ln
                prev_cut = cut.current_cut
                prev_indent = cur_indent
                if output_cut:
                    print(lp, file=out, end="")

    if args.cut_counting:
        print(cut.current_cut, file=out)
