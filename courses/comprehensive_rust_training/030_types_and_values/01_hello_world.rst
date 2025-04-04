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

-  Functions are introduced with :rust:`fn`.
-  Blocks are delimited by curly braces like in C and C++.
-  The :rust:`main` function is the entry point of the program.
-  Rust has hygienic macros, :rust:`println!` is an example of this.
-  Rust strings are UTF-8 encoded and can contain any Unicode character.

---------
Details
---------

This slide tries to make the students comfortable with Rust code. They
will see a ton of it over the next four days so we start small with
something familiar.

Key points:

-  Rust is very much like other languages in the C/C++/Java tradition.
   It is imperative and it doesn't try to reinvent things unless
   absolutely necessary.

-  Rust is modern with full support for things like Unicode.

-  Rust uses macros for situations where you want to have a variable
   number of arguments (no function
   :url:`overloading <../control-flow-basics/functions.md>`).

-  Macros being 'hygienic' means they don't accidentally capture
   identifiers from the scope they are used in. Rust macros are actually
   only `partially
   hygienic <https://veykril.github.io/tlborm/decl-macros/minutiae/hygiene.html>`__.

-  Rust is multi-paradigm. For example, it has powerful `object-oriented
   programming
   features <https://doc.rust-lang.org/book/ch17-00-oop.html>`__, and,
   while it is not a functional language, it includes a range of
   `functional
   concepts <https://doc.rust-lang.org/book/ch13-00-functional-features.html>`__.
