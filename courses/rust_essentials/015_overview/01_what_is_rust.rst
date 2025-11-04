===============
What is Rust?
===============

---------------
What is Rust?
---------------

- Rust is a new(er) programming language

   - First stable release in 2015 (1.0)
   - Modern design to solve older language problems

-  Statically compiled 

   -  :rust:`rustc` uses LLVM as its backend

-  Rust supports many platforms and architectures

   -  Linux, Windows, VxWorks...
   -  x86, ARM ...   

-  Rust is used for a wide range of devices

   -  embedded
   -  smart displays
   -  mobile phones
   -  desktops
   -  servers

--------------------------------
What Kind of Language is Rust?
--------------------------------

Rust fits in the same area as other systems languages (Ada, C++, ...)

-  High flexibility
-  High level of control
-  Can be scaled down to very constrained devices such as
   microcontrollers
-  Has no runtime or garbage collection
-  Focuses on reliability and safety without sacrificing performance

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
