==================
Unsafe Functions
==================

-----------------------
Declaring and Calling
-----------------------

An unsafe function has preconditions that the compiler cannot check

* Declared with :rust:`unsafe fn`
* Caller must uphold its documented safety contract
* Call requires an unsafe context

.. code:: rust

  /// Performs an operation whose preconditions are not compiler-checked.
  ///
  /// # Safety
  ///
  /// The caller must uphold the function's documented requirements.
  unsafe fn dangerous() {
      // Implementation goes here
  }

  fn main() {
      // dangerous(); // Compile error: call requires an unsafe block

      // SAFETY: The requirements documented by `dangerous` are satisfied.
      unsafe {
          dangerous();
      }
  }

-----------------
Safety Contracts
-----------------

Declaring an unsafe function moves a proof obligation to the caller

* The :rust:`# Safety` documentation explains the required invariants
* Each call site should explain why those requirements are satisfied
* An unsafe function does not make every operation in its body acceptable

  * In Rust 2024, unsafe operations inside it should use explicit unsafe blocks

.. note::

  :rust:`unsafe fn` describes a contract at the function boundary.
  :rust:`unsafe { ... }` marks the specific operations that rely on that contract.

----------------------------------
Foreign Function Interface (FFI)
----------------------------------

FFI interacts with code written in other languages, most commonly C

* Rust cannot verify the implementation of an external binary
* In Rust 2024, an external block must be declared :rust:`unsafe extern`
* External functions are unsafe by default unless explicitly declared safe

.. code:: rust

  use std::ffi::c_int;

  unsafe extern "C" {
      fn abs(input: c_int) -> c_int;
  }

  fn main() {
      // SAFETY: The declaration matches C's `abs`, and `-3` is not the
      // minimum representable `c_int`, whose absolute value could overflow.
      unsafe {
          println!("Absolute value of -3 according to C: {}", abs(-3));
      }
  }
