==============
Introduction
==============

----------------
Topics Covered
----------------

* **Memory Budgets**

  - Distinguish storage location, capacity, and total memory use

* **Dynamic Allocation**

  - Identify allocation costs and handle errors from fallible allocations

* **Caller-Provided and Static Storage**

  - Reuse existing memory and control access to persistent data

* **Fixed-Capacity Collections**

  - Store bounded data without growing a heap allocation

* **Runtime Constraints**

  - Choose limits, failure policies, and checks that match requirements

-------------------------------------
Available Does Not Mean Appropriate
-------------------------------------

* :rust:`no_std` does not set limits on memory use

* An allocator makes heap-backed types usable, not unlimited

* A design must also answer three questions:

  * How much memory can be used at once?

  * When can allocation or other costly work occur?

  * What should happen when memory limit is reached?

.. warning::

  Memory safety does not bound memory use or execution time

-----------------------------------------
Running Example: Sorting Sample Batches
-----------------------------------------

**Compare three ways to sort** :rust:`u16` **samples**

* Return a heap-backed collection, handling reservation failure

* Write into a buffer supplied by the caller

* Return an owned, fixed-capacity collection

------------------------------
Example Library: Cargo Setup
------------------------------

**A standalone library hosts the examples**

* Creation of the :filename:`memory_tools` library

.. code:: sh

  cargo new --lib memory_tools

* Feature configuration in :filename:`Cargo.toml`

.. code:: toml

  [features]
  default = ["std"]
  std = ["alloc"]
  alloc = []

-------------------------------
Example Library: Source Files
-------------------------------

**The library declares a separate module for the examples**

* Library root in :filename:`src/lib.rs`

.. code:: rust

  #![cfg_attr(not(feature = "std"), no_std)]

  #[cfg(feature = "alloc")]
  extern crate alloc;

  pub mod memory;

* :filename:`src/memory.rs` starts empty

  * Later slides add its implementations
