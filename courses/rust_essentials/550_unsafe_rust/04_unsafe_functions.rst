==================
Unsafe Functions
==================

-----------------------
Declaring and Calling
-----------------------

**Use an unsafe function when callers must uphold unchecked preconditions**

* Declared with :rust:`unsafe fn`
* Caller must satisfy the documented safety contract
* Calls require an unsafe context

.. code:: rust

  /// # Safety
  ///
  /// Callers must satisfy the documented requirements.
  unsafe fn dangerous() {
      // Implementation goes here
  }

  fn main() {
      // dangerous(); // Error: requires an unsafe block

      // SAFETY: All documented requirements hold.
      unsafe {
          dangerous();
      }
  }

------------------
Safety Contracts
------------------

**An unsafe function transfers a proof obligation to its caller**

* :rust:`# Safety` documents the required invariants
* Each call site should explain why they hold
* Unsafe operations in the function body remain unsafe

  * Rust 2024 expects explicit unsafe blocks around them

.. note::

  :rust:`unsafe fn` defines a boundary contract; :rust:`unsafe { ... }` marks the operations that rely on it.

----------------------------------
Foreign Function Interface (FFI)
----------------------------------

**FFI calls code written in another language, commonly C**

* Rust cannot verify the external implementation
* Rust 2024 requires :rust:`unsafe extern` blocks
* External functions are unsafe unless declared safe

.. code:: rust

  use std::ffi::c_int;

  unsafe extern "C" {
      fn abs(input: c_int) -> c_int;
  }

  fn main() {
      // SAFETY: The declaration matches C `abs`;
      // '-3' is a valid 'c_int' input.
      unsafe {
          println!("abs(-3): {}", abs(-3));
      }
  }
