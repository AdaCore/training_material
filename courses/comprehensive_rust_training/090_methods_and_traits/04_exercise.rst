========================
Exercise: Logger Trait
========================

------------------------
Exercise: Logger Trait
------------------------

Let's design a simple logging utility, using a trait :rust:`Logger` with a
:rust:`log` method. Code which might log its progress can then take an
:rust:`&impl Logger`. In testing, this might put messages in the test
logfile, while in a production build it would send messages to a log
server.

However, the :rust:`StderrLogger` given below logs all messages, regardless
of verbosity. Your task is to write a :rust:`VerbosityFilter` type that will
ignore messages above a maximum verbosity.

This is a common pattern: a struct wrapping a trait implementation and
implementing that same trait, adding behavior in the process. What other
kinds of wrappers might be useful in a logging utility?

::

   {{#include exercise.rs:setup}}

   // TODO: Define and implement `VerbosityFilter`.

   {{#include exercise.rs:main}}
