==================
Unsafe Functions
==================

------------------
Safety Contracts
------------------

**An unsafe function transfers a safety obligation to its caller**

* The function documents requirements callers must uphold
* :rust:`# Safety` is the conventional Rustdoc section for those requirements
* Each call site should explain why the requirements hold

.. note::

  :rust:`unsafe fn` defines a contract; calls require an unsafe context

------------------------------
Declaring an Unsafe Function
------------------------------

**Use an unsafe function when safety depends on caller guarantees**

* Declared with :rust:`unsafe fn`
* Caller must satisfy the documented safety contract

.. code:: rust

  /// # Safety
  ///
  /// Callers must satisfy the documented requirements
  unsafe fn dangerous() {
      // Implementation goes here
  }

----------------------------
Calling an Unsafe Function
----------------------------

**Calling an unsafe function requires an unsafe context**

.. code:: rust

  fn main() {
      // dangerous(); // Error: requires an unsafe block

      // SAFETY: All documented requirements hold
      unsafe {
          dangerous();
      }
  }

.. note::

  Rust 2024 warns on unsafe operations in :rust:`unsafe fn` without blocks

--------------------------------
Declaring an External Function
--------------------------------

**FFI calls code written in another language, commonly C**

* Rust cannot verify the external implementation
* Rust 2024 requires :rust:`unsafe extern` blocks
* External functions are unsafe unless declared safe

.. code:: rust

  use std::ffi::c_int;

  unsafe extern "C" {
      fn abs(input: c_int) -> c_int;
  }

------------------------------
Calling an External Function
------------------------------

**Calling an external function requires an unsafe context**

.. code:: rust

  fn main() {
      let meaning_of_life: c_int = -42;

      // SAFETY: The declaration matches C 'abs',
      // and 'abs(-42)' is representable as 'c_int'
      let answer = unsafe { abs(meaning_of_life) };

      println!("The answer is: {answer}");
  }

.. code:: output

  The answer is: 42
