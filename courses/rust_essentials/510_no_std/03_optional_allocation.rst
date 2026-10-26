=====================
Optional Allocation
=====================

--------------------------------
"no_std" Does Not Mean No Heap
--------------------------------

* The :rust:`alloc` crate supplies heap-backed types without requiring :rust:`std`

* :rust:`alloc` ships with the toolchain; it is not a :filename:`crates.io` dependency

* A reusable library can use those *types* without selecting an allocator

* Final program must supply suitable allocation support

  * A normal :rust:`std` application already supplies a default global allocator

.. warning::

  :rust:`extern crate alloc` makes the crate available. It does not create a heap or install an allocator

-----------------------------------
Stack, Static Storage, and Heap
-----------------------------------

.. image:: rust_essentials/510_stack_static_heap.svg
   :width: 100%
   :align: center

* Local variables usually live in a stack frame

* :rust:`static` items live for the whole program

* A :rust:`Vec` header stores a pointer, a length, and a capacity

* The elements of a :rust:`Vec` live in heap-backed storage

-----------------------------------------
An Allocation-Requiring Convenience API
-----------------------------------------

**Add these items, keeping** :rust:`#![no_std]` **at the top**

.. code:: rust

  extern crate alloc;

  use alloc::vec::Vec;

  pub fn sorted_samples(samples: &[u16]) -> Vec<u16> {
      let mut sorted = Vec::from(samples);
      sorted.sort_unstable();
      sorted
  }

* The function returns an owned, sorted copy of the input

* The feature section will make this API optional

-----------------------------------------
Types and Macros Need Their Own Imports
-----------------------------------------

**With** :rust:`alloc` **available, an independent example can use these imports**

.. code:: rust

  use alloc::string::String;
  use alloc::vec::Vec;
  use alloc::{format, vec};

  pub fn make_message() -> (Vec<u16>, String) {
      let values = vec![10, 42, 7];
      let text = format!("{} samples", values.len());
      (values, text)
  }

* Importing the :rust:`Vec` type does not import the :rust:`vec!` macro

* :rust:`format!` creates an owned :rust:`String`; :rust:`format_args!` does not
