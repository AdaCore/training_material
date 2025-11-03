==============
Hello, World
==============

--------------
Hello, World
--------------

Let us jump into the simplest possible Rust program, a classic Hello
World program:

.. code:: rust

   fn main() {
       println!("Hello World!");
   }

What you see:

- Functions are introduced with :rust:`fn`.
- Blocks are delimited by curly braces like in C and C++.
- The :rust:`main` function is the entry point of the program.
- Rust has hygienic macros, :rust:`println!` is an example of this.
- Rust strings are UTF-8 encoded and can contain any Unicode character.

-------------------------------
Things To Consider About Rust
-------------------------------

- Very much like other languages in the C/C++/Java tradition.

  - Imperative
  - No reinventing things unless absolutely necessary.

- Modern with full support for things like Unicode.
- Uses macros for situations where you want to have a variable number of arguments

  - (no function :url:`overloading <../control-flow-basics/functions.md>`).

- Macros being 'hygienic' means

  - Don't accidentally capture identifiers from the scope they are used in
  - Macros are actually only :url:`partially hygienic <https://veykril.github.io/tlborm/decl-macros/minutiae/hygiene.html>`

- Multi-paradigm

  - Powerful :url:`object-oriented programming features <https://doc.rust-lang.org/book/ch17-00-oop.html>`
  - Not a function language, but includes a range of :url:`functional concepts <https://doc.rust-lang.org/book/ch13-00-functional-features.html>`
