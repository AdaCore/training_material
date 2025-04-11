===========================
Compiler Lints and Clippy
===========================

---------------------------
Compiler Lints and Clippy
---------------------------

The Rust compiler produces fantastic error messages, as well as helpful
built-in lints. :url:`Clippy <https://doc.rust-lang.org/clippy/>` provides
even more lints, organized into groups that can be enabled per-project.

.. container:: latex_environment small

   .. code:: rust
      :number-lines: 1

      #[deny(clippy::cast_possible_truncation)]
      fn main() {
          let mut x = 3;
          while (x < 70000) {
              x *= 2;
          }
          println!("X probably fits in a u16, right? {}", x as u16);
      }

---------------
Lint Messages
---------------

- Compiler lints

.. container:: latex_environment small

   .. code:: rust
      :number-lines: 4

      while (x < 70000) {


   ::

      warning: unnecessary parentheses around `while` condition

- Clippy lints

.. container:: latex_environment small

   .. code:: rust
      :number-lines: 7

      println!("X probably fits in a u16, right? {}", x as u16);

   ::

      error: casting `i32` to `u16` may truncate the value

Clippy has extensive documentation of its lints, and adds new lints (including
default-deny lints) all the time.
