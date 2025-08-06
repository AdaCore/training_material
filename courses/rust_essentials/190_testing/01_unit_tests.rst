============
Unit Tests
============

------------
Unit Tests
------------

Rust and Cargo come with a simple unit test framework. Tests are marked
with :rust:`#[test]`. Unit tests are often put in a nested :rust:`tests` module,
using :rust:`#[cfg(test)]` to conditionally compile them only when building
tests.

.. code:: rust

   fn first_word(text: &str) -> &str {
       match text.find(' ') {
           Some(idx) => &text[..idx],
           None => &text,
       }
   }

   #[cfg(test)]
   mod tests {
       use super::*;

       #[test]
       fn test_empty() {
           assert_eq!(first_word(""), "");
       }

       #[test]
       fn test_single_word() {
           assert_eq!(first_word("Hello"), "Hello");
       }

       #[test]
       fn test_multiple_words() {
           assert_eq!(first_word("Hello World"), "Hello");
       }
   }

-  This lets you unit test private helpers.
-  The :rust:`#[cfg(test)]` attribute is only active when you run
   :rust:`cargo test`.

---------
Details
---------

Run the tests in the playground in order to show their results.
