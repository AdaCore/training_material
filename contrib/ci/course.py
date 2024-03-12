import json
import os
import argparse
from pathlib import Path


SINGLE_COURSE_FILE = Path(__file__).resolve().parents[2] / "DO_NOT_MERGE.SINGLE_COURSE"


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("action", choices=["find"])
    ap.add_argument("dir", type=Path)
    ap.add_argument("--json", action="store_true")
    args = ap.parse_args()

    assert args.dir.is_dir()

    if args.action == "find":
        # courses are all the direct subdirs that contain a course.toml file,
        # and any txt files in those dirs is a course variant
        if SINGLE_COURSE_FILE.is_file():
            with open(SINGLE_COURSE_FILE) as f:
                courses = [args.dir / f.read().strip()]
        else:
            courses = [d.parent for d in args.dir.glob("**/course.toml")]
            courses += [f for d in courses for f in d.glob("*.txt")]

        courses_str = [str(c.relative_to(args.dir)) for c in courses]

        if args.json:
            print(json.dumps(courses_str))
        else:
            print(os.linesep.join(courses_str))
