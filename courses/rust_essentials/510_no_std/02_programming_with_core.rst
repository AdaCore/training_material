=========================
Programming With "core"
=========================

--------------------------------
What Still Works With "no_std"?
--------------------------------

* Ownership, borrowing, and lifetimes
* Structs, enums, pattern matching, and methods
* Generics, traits, and closures
* Arrays, slices, and :rust:`str`

.. note::

  Available library APIs depend on the crates in use

----------------------
What "core" Provides
----------------------

.. list-table::
  :header-rows: 1
  :widths: 44 56

  * - **Functionality**
    - **Examples**
  * - *Optional values and errors*
    - :rust:`Option`, :rust:`Result`
  * - *Iteration*
    - :rust:`Iterator`, :rust:`map`, :rust:`filter`, :rust:`fold`
  * - *Common traits*
    - :rust:`Copy`, :rust:`Clone`, :rust:`Default`, :rust:`Drop`
  * - *Formatting and errors*
    - :rust:`Debug`, :rust:`Display`, :rust:`core::error::Error`
  * - *Memory utilities*
    - :rust:`core::mem`, :rust:`Cell`, :rust:`RefCell`
  * - *Macros*
    - :rust:`panic!`, :rust:`assert!`, :rust:`assert_eq!`

------------------------------
What "core" Does Not Provide
------------------------------

.. list-table::
  :header-rows: 1
  :widths: 32 68

  * - **Familiar API**
    - **Without** :rust:`std`
  * - :rust:`Vec`, :rust:`String`, :rust:`Box`
    - Use :rust:`alloc`, or avoid heap-backed storage
  * - :rust:`println!`, :rust:`eprintln!`
    - Unavailable without :rust:`std`; use an alternative output mechanism
  * - :rust:`std::thread`
    - No standard OS-thread API is provided
  * - :rust:`std::fs`, socket APIs
    - File and network I/O require alternative APIs

.. note::

  I/O and concurrency remain possible without :rust:`std` via other APIs

---------------------------
Designing Portable APIs
---------------------------

**Portable APIs can avoid heap allocation and platform services**

* :rust:`&[T]` and :rust:`&str` allow functions to borrow caller-owned data
* :rust:`Option` and :rust:`Result` represent absent values and recoverable errors
* Iterators and error propagation with :rust:`?` work without :rust:`std`
* Callers decide how to handle output and other platform services

--------------------------
A Small Portable Library
--------------------------

**Library** :rust:`sample_tools` **uses slices and iterators without** :rust:`std`

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

**String slices and error propagation remain available without** :rust:`std`

.. code:: rust

  use core::num::ParseIntError;

  pub fn parse_pair(left: &str, right: &str)
      -> Result<[u16; 2], ParseIntError> {
      let first: u16 = left.trim().parse()?;
      let second: u16 = right.trim().parse()?;
      Ok([first, second])
  }

* :rust:`trim()` returns a borrowed subslice without allocating

.. note::

  :rust:`Result` and :rust:`?` work without :rust:`std`

-----------------------------------
Formatting Is Not Standard Output
-----------------------------------

**Formatting is available through** :rust:`core` **without standard output**

.. list-table::
  :header-rows: 1
  :widths: 34 66

  * - **API**
    - **Role**
  * - :rust:`Debug`, :rust:`Display`
    - Formatting traits in :rust:`core::fmt`
  * - :rust:`format_args!`
    - Prepares formatting arguments without creating a :rust:`String`
  * - :rust:`write!`, :rust:`writeln!`
    - Format into a supplied destination

.. note::

  Describing text and choosing where to send it are separate responsibilities

--------------------------------------------
Using the Library From a "std" Application
--------------------------------------------

**Host applications using** :rust:`std` **can call** :rust:`no_std` **libraries**

.. code:: rust

  use sample_tools::{maximum, parse_pair};

  fn main() {
      let samples = parse_pair("10", " 42 ")
          .expect("valid samples");
      if let Some(value) = maximum(&samples) {
          println!("Maximum: {value}");
      }
  }

* Application provides :rust:`main` and prints ``Maximum: 42``
* Library remains :rust:`no_std` when called from a :rust:`std` application

-------------------------------
Let Caller Choose Destination
-------------------------------

**Formatting can use a caller-supplied writer**

.. code:: rust

  use core::fmt::{self, Write};

  pub fn write_maximum(
      output: &mut impl Write,
      value: u16,
  ) -> fmt::Result {
      write!(output, "Maximum: {value}")
  }

* :rust:`output` accepts any writer implementing :rust:`core::fmt::Write`
* Write failures are returned through :rust:`fmt::Result`
* Library code creates no intermediate :rust:`String`

  * The writer may still allocate

-----------------------------------
The Caller Can Still Use "String"
-----------------------------------

**Callers can choose an allocating output destination**

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
