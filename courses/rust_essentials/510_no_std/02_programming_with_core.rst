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

* Macros such as :rust:`panic!`, :rust:`assert!`, and :rust:`assert_eq!` remain available

.. note::

  The final program determines how panics are handled

--------------------------
A Small Portable Library
--------------------------

**Example library:** :filename:`sample_tools/src/lib.rs`

.. code:: rust

  #![no_std]

  pub fn maximum(samples: &[u16]) -> Option<u16> {
      samples.iter().copied().max()
  }

* Function borrows its input and returns

  * :rust:`None` for an empty slice
  * :rust:`Some` with largest element otherwise

* No heap allocation or platform service is needed

--------------------------------------
Borrowed Text and Recoverable Errors
--------------------------------------

**Additional function in** :filename:`src/lib.rs`

.. code:: rust

  use core::num::ParseIntError;

  pub fn parse_pair(
      left: &str,
      right: &str,
  ) -> Result<[u16; 2], ParseIntError> {
      let first: u16 = left.trim().parse()?;
      let second: u16 = right.trim().parse()?;
      Ok([first, second])
  }

* This function returns two numbers or propagates a parsing error

* :rust:`&str` is borrowed text; :rust:`trim()` returns a subslice without allocating

* :rust:`Result` and :rust:`?` work without :rust:`std`

--------------------------------------------
Using the Library From a "std" Application
--------------------------------------------

**Host-side example in** :filename:`sample_tools/examples/inspect.rs`

.. code:: rust

  use sample_tools::{maximum, parse_pair};

  fn main() {
      let samples = parse_pair("10", " 42 ")
          .expect("valid samples");
      if let Some(value) = maximum(&samples) {
          println!("Maximum: {value}");
      }
  }

* :command:`cargo run --example inspect` prints ``Maximum: 42``

* Example application uses :rust:`std`; the library remains :rust:`no_std`

------------------------------
What "core" Does Not Provide
------------------------------

.. list-table::
  :header-rows: 1
  :widths: 32 68

  * - Familiar API
    - Without :rust:`std`
  * - :rust:`Vec`, :rust:`String`, :rust:`Box`
    - Use :rust:`alloc`, or avoid heap-backed storage
  * - :rust:`println!`, :rust:`eprintln!`
    - Let the caller choose how to produce output
  * - :rust:`std::thread`
    - No standard OS-thread API is provided
  * - :rust:`std::fs`, socket APIs
    - Require another implementation or a caller-supplied interface

.. note::

  Losing these :rust:`std` APIs does not make I/O or concurrency impossible. The required support must come from elsewhere

-----------------------------------
Formatting Is Not Standard Output
-----------------------------------

* :rust:`Debug` and :rust:`Display` are available through :rust:`core::fmt`

* :rust:`format_args!` prepares formatting arguments without creating a :rust:`String`

* :rust:`write!` and :rust:`writeln!` format into a supplied destination

* :rust:`format_args!`, :rust:`write!`, and :rust:`writeln!` work without :rust:`std`

* :rust:`println!` requires :rust:`std` to write to standard output

.. tip::

  Describing text and choosing where to send it are separate responsibilities

-------------------------------
Let Caller Choose Destination
-------------------------------

**Function** :rust:`write_maximum` **defined in** :filename:`src/lib.rs`

.. code:: rust

  use core::fmt::{self, Write};

  pub fn write_maximum(
      output: &mut impl Write,
      value: u16,
  ) -> fmt::Result {
      write!(output, "Maximum: {value}")
  }

* :rust:`output: &mut impl Write` accepts a caller-supplied writer implementing :rust:`core::fmt::Write`

* Write failures are returned through :rust:`fmt::Result`

* No intermediate :rust:`String`

  * Writer may allocate

-----------------------------------
The Caller Can Still Use "String"
-----------------------------------

**Second host-side example in** :filename:`examples/report.rs`

.. code:: rust

  use sample_tools::write_maximum;

  fn main() {
      let mut text = String::new();
      write_maximum(&mut text, 42)
          .expect("formatting succeeded");
      println!("{text}");
  }

* Application chooses an allocating destination

* Library code does not need to name or depend on :rust:`String`

* Another caller can provide a different writer
