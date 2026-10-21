======================
Checking and Testing
======================

----------------------------
Testing a "no_std" Library
----------------------------

* The standard test harness uses :rust:`std` on the development machine

* The library keeps the selected feature configuration

* Test the core API in each supported configuration

.. note::

  The host harness does not enable the library's :rust:`std` feature

-------------------------------------------
Ordinary Tests on the Development Machine
-------------------------------------------

**Add to** :filename:`src/lib.rs`

.. code:: rust

  #[cfg(test)]
  mod tests {
      use super::maximum;

      #[test]
      fn finds_the_maximum() {
          assert_eq!(maximum(&[10, 42, 7]), Some(42));
          assert_eq!(maximum(&[]), None);
      }
  }

------------------------------------
Check Each Supported Configuration
------------------------------------

**Run from the** :filename:`sample_tools` **directory**

.. code:: sh

  cargo check --lib --no-default-features
  cargo check --lib --no-default-features --features alloc
  cargo check --lib

* Run tests for the same three configurations

.. code:: sh

  cargo test --lib --no-default-features
  cargo test --lib --no-default-features --features alloc
  cargo test --lib

* These cover the core-only, allocating, and default :rust:`std` builds

* :command:`--all-features` alone would miss the smallest configurations

-------------------------------
What Host Checks Do Not Prove
-------------------------------

* A host build may succeed even if a dependency still uses :rust:`std`

* The standard test harness itself links :rust:`std`

* :command:`cargo check` checks code but does not produce a final linked program

* :rust:`no_std` alone does not prove portability to every platform

.. tip::

  Test behavior, review dependencies, and document the capabilities each library configuration requires

-----------------------
What We Did Not Cover
-----------------------

* **Memory and Runtime Constraints**

  - Allocation costs and failure handling

  - Static storage and fixed-capacity collections

* **Executable Runtime Setup**

  - Entry points, linker configuration, and panic-handler implementation

  - Installing or implementing an allocator

* **Platform Integration**

  - Concrete I/O backends and target-specific services

.. tip::

  Knowing that :rust:`alloc` is available is different from deciding whether allocation fits an application's constraints
