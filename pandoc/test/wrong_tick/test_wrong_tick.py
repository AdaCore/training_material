import pytest
from pathlib import Path
import epycs.subprocess
import tempfile

CURDIR = Path(__file__).resolve().parent
ROOT = CURDIR.parents[2]
PANDOC_FE = ROOT / "pandoc" / "pandoc_fe.py"

INPUT = CURDIR / "input.rst"
EXPECTED_LINES = CURDIR / "output.lines"

pandoc_fe = epycs.subprocess.cmd.python.arg(PANDOC_FE).with_default_kw(cwd=ROOT)


def test_tex_generation_with_backtick():
    with tempfile.NamedTemporaryFile(suffix=".tex") as tout:
        OUTPUT = Path(tout.name)
        pandoc_fe("--source", INPUT, "--extension", "tex", "--output", OUTPUT)
        with open(tout.name, "rt") as ftout:
            actual = ftout.read()
    with open(EXPECTED_LINES, "rt") as ftexp:
        expected_lines = [line.strip() for line in ftexp.readlines()]

    actual_lines = [line.strip() for line in actual.splitlines()]
    errors = []
    for line in expected_lines:
        if line not in actual_lines:
            errors.append(line)

    assert not errors, f"lines {errors!r} not in {actual}"
