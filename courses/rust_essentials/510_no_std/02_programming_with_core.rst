=========================
Programming with "core"
=========================

---------------------------
Familiar Rust Still Works
---------------------------

* Ownership, borrowing, and lifetimes still apply

* Structs, enums, pattern matching, and methods still work

* Generics, traits, and closures remain available

* Arrays, slices, and :rust:`str` remain part of the language

.. tip::

  These are language features. :rust:`core` provides many of the library types, methods, and trait implementations used with them

----------------------
What "core" Provides
----------------------

.. list-table::
  :header-rows: 1
  :widths: 44 56

  * - Familiar functionality
    - Examples
  * - Optional values and errors
    - :rust:`Option`, :rust:`Result`
  * - Iteration
    - :rust:`Iterator`, :rust:`map`, :rust:`filter`, :rust:`fold`
  * - Common traits
    - :rust:`Copy`, :rust:`Clone`, :rust:`Default`, :rust:`Drop`
  * - Formatting and errors
    - :rust:`Debug`, :rust:`Display`, :rust:`core::error::Error`
  * - Memory utilities
    - :rust:`core::mem`, :rust:`Cell`, :rust:`RefCell`

* :rust:`panic!` and assertion macros also remain available

.. note::

  The final program determines how panics are handled

--------------------------
A Small Portable Library
--------------------------

**Create a library package named** :rust:`sample_tools`

.. code:: sh

  cargo new --lib --edition 2024 sample_tools
  cd sample_tools

* Replace :filename:`src/lib.rs` with

.. code:: rust

  #![no_std]

  pub fn maximum(samples: &[u16]) -> Option<u16> {
      samples.iter().copied().max()
  }

* The function borrows its input and returns :rust:`None` for an empty slice

* No heap allocation or platform service is needed

--------------------------------------
Borrowed Text and Recoverable Errors
--------------------------------------

**Add to** :filename:`src/lib.rs`

.. code:: rust

  use core::num::ParseIntError;

  pub fn parse_pair(
      left: &str,
      right: &str,
  ) -> Result<[u16; 2], ParseIntError> {
      Ok([left.trim().parse()?, right.trim().parse()?])
  }

* :rust:`&str` and methods such as :rust:`trim` do not require an owned :rust:`String`

* :rust:`Result` and :rust:`?` work without :rust:`std`

* This function returns two numbers or propagates a parsing error

--------------------------------------------
Using the Library from a "std" Application
--------------------------------------------

**Create** :filename:`examples/inspect.rs` **inside** :filename:`sample_tools`

.. code:: rust

  use sample_tools::{maximum, parse_pair};

  fn main() {
      let samples = parse_pair("10", " 42 ")
          .expect("valid samples");
      if let Some(value) = maximum(&samples) {
          println!("Maximum: {value}");
      }
  }

* Run :command:`cargo run --example inspect` to print ``Maximum: 42``

* The example uses :rust:`std`; the library still uses :rust:`no_std`

-------------------------------
What "std" No Longer Provides
-------------------------------

.. list-table::
  :header-rows: 1
  :widths: 32 68

  * - Familiar API
    - Without :rust:`std`
  * - :rust:`Vec`, :rust:`String`, :rust:`Box`
    - Use :rust:`alloc`, or avoid heap-backed storage
  * - :rust:`println!`, :rust:`eprintln!`
    - Let the caller choose how to produce output
  * - :rust:`std::fs`, socket APIs
    - Require another implementation or a caller-supplied interface
  * - :rust:`std::thread`
    - No standard OS-thread API is provided

.. note::

  Losing these :rust:`std` APIs does not make I/O or concurrency impossible. The required support must come from elsewhere

-----------------------------------
Formatting Is Not Standard Output
-----------------------------------

* :rust:`Debug` and :rust:`Display` are available through :rust:`core::fmt`

* :rust:`format_args!` prepares formatting arguments without creating a :rust:`String`

* :rust:`write!` and :rust:`writeln!` format into a supplied destination

* :rust:`println!` additionally assumes access to standard output

.. tip::

  Describing text and choosing where to send it are separate responsibilities

---------------------------------------
Let the Caller Choose the Destination
---------------------------------------

**Add to** :filename:`src/lib.rs`

.. code:: rust

  use core::fmt::{self, Write};

  pub fn write_maximum(
      output: &mut impl Write,
      value: u16,
  ) -> fmt::Result {
      write!(output, "Maximum: {value}")
  }

* The writer's type implements :rust:`core::fmt::Write`

* Write failures are returned through :rust:`fmt::Result`

* No intermediate :rust:`String`; the writer may allocate

-----------------------------------
The Caller Can Still Use "String"
-----------------------------------

**Create a separate host example in** :filename:`examples/report.rs`

.. code:: rust

  use sample_tools::write_maximum;

  fn main() {
      let mut text = String::new();
      write_maximum(&mut text, 42)
          .expect("formatting succeeded");
      println!("{text}");
  }

* The application chooses an allocating destination

* The library does not need to name or depend on :rust:`String`

* Another caller can provide a different writer
