=====================
Flexible Allocation
=====================

-------------------------------
Heap Allocation With "no_std"
-------------------------------

* :rust:`alloc` supplies heap-backed types without requiring :rust:`std`

  * Provided by the Rust toolchain
  * No crates.io dependency is needed in :filename:`Cargo.toml`

* Reusable libraries can use those types *without* selecting an allocator
* Final program must supply suitable allocation support

  * :rust:`std` applications normally provide a default global allocator
  * Bare-metal applications using heap-backed types must provide a global allocator

.. warning::

  :rust:`extern crate alloc` makes the crate available; it does *not* create a heap or install an allocator

---------------------------
Using "Vec" With "no_std"
---------------------------

**A** :rust:`no_std` **library can use** :rust:`Vec` **without choosing an allocator**

.. code:: rust

  extern crate alloc;

  use alloc::vec::Vec;

  pub fn sorted_samples(samples: &[u16]) -> Vec<u16> {
      let mut sorted = Vec::from(samples);
      sorted.sort_unstable();
      sorted
  }

* The imports make :rust:`Vec` available without :rust:`std`
* :rust:`Vec::from(samples)` creates an owned copy, using the final program's allocator when allocation is needed

-----------------------------------
Macros Can Allocate With "no_std"
-----------------------------------

**Macros can create heap-backed values even without** :rust:`std`

.. code:: rust

  extern crate alloc;

  use alloc::{format, string::String, vec, vec::Vec};

  pub fn make_message() -> (Vec<u16>, String) {
      let values = vec![10, 42, 7];
      let text = format!("{} samples", values.len());
      (values, text)
  }

* :rust:`vec!` creates a heap-backed :rust:`Vec`
* :rust:`format!` creates a heap-backed :rust:`String`
* Both require allocator support in the final program
