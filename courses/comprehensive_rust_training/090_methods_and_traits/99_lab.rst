========================
Exercise: Logger Trait
========================

------------------------
Logger Trait Problem
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

.. code:: rust

   trait Logger {
       /// Log a message at the given verbosity level.
       fn log(&self, verbosity: u8, message: &str);
   }

   struct StderrLogger;

   impl Logger for StderrLogger {
       fn log(&self, verbosity: u8, message: &str) {
           eprintln!("verbosity={verbosity}: {message}");
       }
   }

   /// Only log messages up to the given verbosity level.
   struct VerbosityFilter {
       max_verbosity: u8,
       inner: StderrLogger,
   }

   // TODO: Define and implement `VerbosityFilter`.

   fn main() {
       let logger = VerbosityFilter { max_verbosity: 3, inner: StderrLogger };
       logger.log(5, "FYI");
       logger.log(2, "Uhoh");
   }

------------------------
Logger Trait Solution
------------------------

.. code:: rust

   impl Logger for VerbosityFilter {
       fn log(&self, verbosity: u8, message: &str) {
           if verbosity <= self.max_verbosity {
               self.inner.log(verbosity, message);
           }
       }
   }
