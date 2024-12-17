import argparse
import toml
from pathlib import Path
import epycs.subprocess as esubp
import difflib

scripts_dir = Path(__file__).parent
adacut = esubp.cmd.adacut

esubp.verbose = True


class FileList:
    def __init__(self, path):
        with open(path) as f:
            self.toml = toml.load(f)

        self.extract = self.toml["extract"]

        for name, extract in self.extract.items():
            assert Path(extract["source"]).exists()

    def run_extract(self, outdir, only_check=False):
        for name, extract in self.extract.items():
            source = extract["source"]
            cut_number = extract["cut"]
            out_file_name = (outdir / name).with_suffix(Path(source).suffix)
            if only_check:
                print(out_file_name, end=": ")

                with open(out_file_name, "r") as fout:
                    q_old = fout.read()

                q_new = adacut("-d", f"-c={cut_number}", source, out_filter=str)

                if q_old != q_new:
                    print("\033[31mFAIL\033[0m")
                    check_failed = True
                    diff_lines = list(
                        difflib.unified_diff(
                            q_old.splitlines(),
                            q_new.splitlines(),
                            fromfile=f"{out_file_name} (old)",
                            tofile=f"{out_file_name} (new)",
                        )
                    )

                    pr = lambda s: None
                    color = lambda c: "31" if c == "-" else ("32" if c == "+" else "0")
                    print_colored = lambda s: print(f"\033[{color(s[0])}m{s}\033[0m")
                    for i, l in enumerate(diff_lines):
                        if l:
                            pr(l)
                        if l.startswith("@") or l.startswith("@"):
                            diff_lines = diff_lines[i + 1 :]
                            pr = print_colored
                    print()
                else:
                    print("\033[32mOK\033[0m")
            else:
                with open(out_file_name, "wt") as fout:
                    adacut("-d", f"-c={cut_number}", source, out_filter=fout.write)

    def check_extract(self, outdir):
        self.run_extract(outdir, only_check=True)


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("-C", "--no-check", action="store_true")
    ap.add_argument("filelist", type=Path)
    ap.add_argument("outdir", type=Path, default=Path("out/"))
    args = ap.parse_args()
    fl = FileList(args.filelist)
    if args.no_check:
        fl.run_extract(args.outdir)
    else:
        fl.check_extract(args.outdir)
