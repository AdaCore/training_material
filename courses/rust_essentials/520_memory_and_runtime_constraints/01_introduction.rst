==============
Introduction
==============

----------------
Topics Covered
----------------

* **Memory Budgets**

  - Distinguish storage location, capacity, and total memory use

* **Dynamic Allocation**

  - Identify allocation costs
  - Handle errors from fallible allocations

* **Caller-Provided and Static Storage**

  - Reuse existing memory
  - Control access to persistent data

* **Fixed-Capacity Collections**

  - Store bounded data without growing a heap allocation

* **Runtime Constraints**

  - Choose limits, failure policies, and checks that match requirements

-------------------------------------
Available Does Not Mean Appropriate
-------------------------------------

* Memory budgets apply to stack, static storage, and heap allocations

* An allocator makes heap-backed types usable, not unlimited

* A design must also answer three questions:

  * How much memory can be used at once?

  * When can allocation or other costly work occur?

  * What should happen when memory limit is reached?

.. warning::

  Memory safety does not bound memory use or execution time

------------------------
Sorting Sample Batches
------------------------

**Compare three ways to sort** :rust:`u16` **samples**

* Return a heap-backed collection, handling reservation failure

* Write into a buffer supplied by the caller

* Return an owned, fixed-capacity collection

