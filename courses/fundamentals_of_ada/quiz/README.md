# Content of the directory

This directory contain quizzes.

Quizzes are **templates** for standalone Ada projects with multiple variants.

Given a template, it is concretizable into several projects, one per
choice. Those concrete projects will be *compile-run checked* by the tooling:
those that compile and run without error will be considered correct answers.

There are 3 kinds of errors that can be detected

1. Project does not compile
2. Project does compile but raises a run-time error
3. Project does compile but raises a user error (`-gnata`)

NB: For the case of user errors, the `-gnata` flag will need to be provided in the
associated gpr file.

Warning: Infinite loops or silent failures (return code is 0) are not detected by
the tooling.

# Usage

The quizzes are meant to be used with the `contrib/quiz.py` script. This script
takes an input file as argument and turns it into a Quiz slide in the training
material's ReSTRuctured text idiom.

This is done by instanciating copies of the quiz project directory, one for each
choice (A, B, C...), extracting the associated question, trying a compile-run of
the answer's code, and marking it as an answer in case it compiled and ran
correctly.

The output is saved as a *golden* .rst file and put into version control, so
that the CI can double check the answer (using `contrib/quiz_update.py`), and
validate that the result has not changed unexpectedly.

This allows for reviewers to focus on the rst result, and for maintainers to
focus on the Ada code. As a result, the quiz is reviewed in its included form,
which is not compilable, and the Ada code is saved as well, allowing for
unambiguous reproducibility.

NB: Intermediate results could also be saved for similar reasons, but this is not
the case today.

# Template's format

## adacut format

The template is based on the `adacut` generic format, used by `contrib/adacut.py`.
It provides *directives* that are to be written as

```ada
--$ directive type [target]
```

* `directive` can be any of `begin`, `end`, `line`
* `type` can be any of `cut`, `question`, `answer`
* `target` can be any of `code`, `comment`, `all` (optional, `all` by default)

The directive indicates whether a `begin ... end` block is concerned or just the
line that follows.

```ada
--$ begin cut
Toto : Integer;
Titi : Integer;
--$ end cut

--$ line cut
Toto : Integer;
```

The `cut` type is used to describe and answer, which will be *cut* into the
template for the compile-run check.

First `cut` will be taken as the variant for choice A, second for choice B etc...

If `question` type is used, its content is included as part of the question, as
a `.. code::` rst directive.
If `answer` type is used, its content is included in the **answer** slide as an
explaination paragraph, below the questions.

## Signifiant comments

The comment that is at the start of the file is considered to be the question and
is included as-is (as raw rst).

If a cut **ends** with a comment, it is included as part of the answer in a special
explaination block.
