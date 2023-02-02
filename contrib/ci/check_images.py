#!/usr/bin/env python3
import sys
from pathlib import Path
import itertools

ROOT_DIR = Path(sys.argv[0]).resolve().parents[2]
IMAGES_DIR = ROOT_DIR / "images"


def is_ignored(f):
    return f.stem.lower() == "readme"


def has_lowercase_name(f):
    return f.name.lower() == f.name.lower()


IMAGE_FILE_EXTENSION_LIST = [
    ".jpg",
    ".jpeg",
    ".png",
    ".gif",
    ".webp",
    ".tiff",
    ".psd",
    ".raw",
    ".bmp",
    ".heif",
    ".indd",
    ".svg",
    ".ai",
    ".eps",
    ".pdf",
]


def is_an_image(file_name):
    return file_name.suffix.lower() in IMAGE_FILE_EXTENSION_LIST


def valid_extension(file_name):
    # Use .jpeg, not .jpg
    return file_name.suffix.lower() != ".jpg"


def search_references(f_name, source_files):
    for source in source_files:
        with open(source) as fs:
            yield f_name in fs.read()


def is_referenced(f, source_dir):
    return any(
        search_references(
            f.name,
            itertools.chain(
                source_dir.glob("**/*.rst"),
                source_dir.glob("**/*.md"),
                source_dir.glob("**/*.yaml"),
                source_dir.glob("**/*.latex"),
                source_dir.glob("**/*.sty"),
            ),
        )
    )


fails = 0


def fail(*msg):
    global fails
    fails += 1
    print(*msg, file=sys.stderr)


if __name__ == "__main__":
    for f in IMAGES_DIR.glob("**/*"):
        if is_ignored(f):
            continue

        # nicer image name
        frel = f.relative_to(ROOT_DIR)
        if not has_lowercase_name(f):
            fail("Uppercase   ", f"{frel} is not lower-case")
        elif f.is_dir():
            continue  # File-specific tests below
        elif not is_an_image(f):
            fail("Not An Image", f"{frel} is not an image file")
        elif not valid_extension(f):
            fail("Invalid Type", f"{frel} extension is invalid")
        elif not is_referenced(f, ROOT_DIR):
            fail("No Reference", f"{frel} is not referenced")

    if fails:
        print(f"FAIL: {fails} errors")
    else:
        print("SUCCESS")

    sys.exit(0 if not fails else 1)
