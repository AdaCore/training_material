==============
Introduction
==============

----------------
Topics Covered
----------------

* **Memory Budgets**

  - Distinguish storage location, capacity, and total memory use

* **Dynamic Allocation**

  - Identify allocation costs and handle reported allocation failures

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

  * What should happen when a resource limit is reached?

.. tip::

  Memory safety is not a memory budget or a timing guarantee

-----------------------------------------
Running Example: Sorting Sample Batches
-----------------------------------------

**Extend** :rust:`sample_tools` **with three ways to sort** :rust:`u16` **samples**

* Return a heap-backed collection, handling reservation failure

* Write into a buffer supplied by the caller

* Return an owned, fixed-capacity collection

* Add to :filename:`src/lib.rs`, below its existing crate declarations

.. code:: rust

  pub mod memory;

* Create :filename:`src/memory.rs`; later slides add its contents

.. note::

  Keep the previous module's functions and features unchanged
