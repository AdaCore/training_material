=====================
Optional Allocation
=====================

--------------------------------
"no_std" Does Not Mean No Heap
--------------------------------

* :rust:`alloc` supplies heap-backed types without requiring :rust:`std`

* :rust:`alloc` is provided by the Rust toolchain

* No crates.io dependency on :rust:`alloc` is needed in :filename:`Cargo.toml`

* Reusable libraries can use those types *without* selecting an allocator

* Final program must supply suitable allocation support

  * :rust:`std` applications normally provide a default global allocator

  * Bare-metal applications using heap-backed types must provide a global allocator

.. warning::

  :rust:`extern crate alloc` makes the crate available; it does *not* create a heap or install an allocator

-----------------------------------
Stack, Static Storage, and Heap
-----------------------------------

.. image:: rust_essentials/510_stack_static_heap.svg
   :width: 100%
   :align: center

* Local variables usually live in a stack frame
* Static variables have storage for the whole program
* A :rust:`Vec` header stores a pointer, length, and capacity
* The *elements* of a :rust:`Vec` live in heap-backed storage

-----------------------------------------
An Allocation-Requiring Convenience API
-----------------------------------------

**Additional declarations in** :filename:`src/lib.rs` **below** :rust:`#![no_std]`

.. code:: rust

  extern crate alloc;

  use alloc::vec::Vec;

  pub fn sorted_samples(samples: &[u16]) -> Vec<u16> {
      let mut sorted = Vec::from(samples);
      sorted.sort_unstable();
      sorted
  }

* :rust:`sorted_samples` returns an owned, sorted copy of the input

-----------------------------------------
Types and Macros Need Their Own Imports
-----------------------------------------

**Additional function in** :filename:`src/lib.rs` **reusing the earlier** :rust:`Vec` **import**

.. code:: rust

  use alloc::string::String;
  use alloc::{format, vec};

  pub fn make_message() -> (Vec<u16>, String) {
      let values = vec![10, 42, 7];
      let text = format!("{} samples", values.len());
      (values, text)
  }

* Importing the :rust:`Vec` type does not import the :rust:`vec!` macro

* :rust:`format!` creates an owned :rust:`String`

  * :rust:`format_args!` does not
