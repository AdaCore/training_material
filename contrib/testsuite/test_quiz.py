from pathlib import Path
import epycs.subprocess
from epycs.subprocess import cmd, ShellProgramFilters

epycs.subprocess.exit_on_error = False


TEST_DIR = Path(__file__).parent
TEST_DATA_DIR = TEST_DIR / "quiz"
CONTRIB = TEST_DIR.parent
QUIZ_PY = CONTRIB / "quiz.py"


quiz = cmd.python.arg(QUIZ_PY)


class TestQuiz:
    @classmethod
    def init_tests(cls):
        for d in (
            d_tpl.parent
            for d_tpl in TEST_DATA_DIR.glob("**/template")
            if d_tpl.is_dir()
        ):
            tests = {}

            if (d / "quiz").is_dir():
                cls.add_quiz_test(tests, d, d / "quiz")

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
    def add_quiz_test(cls, tests, d, d_src):
        for f in (d / "template").glob("*.ad?"):

            def test_file_content_is_expected(self, pytestconfig):
                actual = quiz(f, check=True, out_filter=ShellProgramFilters.text)
                self.assert_file_content_equal(
                    pytestconfig, d_src / f"{f.name}.rst", actual
                )

            tests[f"{d_src.parent.name}_{f.name}"] = test_file_content_is_expected


TestQuiz.init_tests()
