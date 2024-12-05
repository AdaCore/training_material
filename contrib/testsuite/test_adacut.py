from pathlib import Path
import epycs.subprocess
from epycs.subprocess import cmd, ShellProgramFilters

epycs.subprocess.exit_on_error = False


TEST_DIR = Path(__file__).parent
TEST_DATA_DIR = TEST_DIR / "adacut"
CONTRIB = TEST_DIR.parent
ADACUT_PY = CONTRIB / "adacut.py"


adacut = cmd.python.arg(ADACUT_PY)


class TestAdaCut:
    @classmethod
    def init_tests(cls):
        for d in (
            d_tpl.parent
            for d_tpl in TEST_DATA_DIR.glob("**/template")
            if d_tpl.is_dir()
        ):
            tests = {}

            for dd in (d_src for d_src in d.glob("src_*") if d_src.is_dir()):
                cls.add_src_tests(tests, d, dd)

            for name, test in tests.items():
                setattr(cls, f"test_{name}", test)

    def maybe_update_baseline(self, pytestconfig, expected_file, actual):
        if pytestconfig.getoption("--update-baseline"):
            with open(expected_file, "wt") as f:
                f.write(actual)

    def assert_file_content_equal(self, pytestconfig, expected_file, actual):
        with open(expected_file) as f:
            expected = f.read()

        try:
            assert actual == expected
        except AssertionError:
            self.maybe_update_baseline(pytestconfig, expected_file, actual)
            raise

    @classmethod
    def parse_options(cls, name):
        return name.split("_")

    @classmethod
    def add_src_tests(cls, tests, d, d_src):
        options = cls.parse_options(d_src.name[len("src_") :])
        for f in (d / "template").glob("*.ad?"):

            def test_file_content_is_expected(self, pytestconfig):
                actual = adacut(
                    *options, "--", f, check=True, out_filter=ShellProgramFilters.text
                )
                self.assert_file_content_equal(pytestconfig, d_src / f.name, actual)

            tests[
                f"{d_src.parent.name}_{d_src.name}_{f.name}"
            ] = test_file_content_is_expected


TestAdaCut.init_tests()
