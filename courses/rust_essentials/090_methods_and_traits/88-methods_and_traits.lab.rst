========================
Exercise: Logger Trait
========================

------------------------
Logger Trait Setup
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

------------------------
Logger Trait Problem
------------------------

.. container:: source_include 090_methods_and_traits/src/090_methods_and_traits.rs :start-after://ANCHOR-setup :end-before://ANCHOR-solution :code:rust

.. code:: rust

   // Implement the Logger trait for VerbosityFilter

.. container:: source_include 090_methods_and_traits/src/090_methods_and_traits.rs :start-after://ANCHOR-main :code:rust

------------------------
Logger Trait Solution
------------------------

.. container:: source_include 090_methods_and_traits/src/090_methods_and_traits.rs :start-after://ANCHOR-solution :end-before://ANCHOR-main :code:rust
