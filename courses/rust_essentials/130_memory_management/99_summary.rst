=========
Summary
=========

-----------------------------
Navigating Memory Semantics
-----------------------------

.. list-table::
  :header-rows: 1

  * - **Mechanism**
    - **Behavior**
    - **Impact**
    - **Trigger**

  * - *Move*
    - Ownership transfer
    - Zero
    - Default for :rust:`non-Copy` types

  * - *Clone*
    - Explicit duplication
    - High
    - Manual :rust:`.clone()` call

  * - *Copy*
    - Implicit duplication
    - Low
    - Automatic for :rust:`Copy` types

-----------------
What We Covered
-----------------

- **Ownership**

  - Single owner rule ensures safety at compile time
  - Scopes determine when data is freed
  
- **Data Movement**

  - **Move**
    - Transfers ownership
  - **Copy**
    - Implicit bitwise copy
  - **Clone**
    - Explicit deep copy for heap-allocated data

- **Cleanup**
  - **Destructors** (Drop): Mechanism for defining custom cleanup logic
  - Resources are cleaned up automatically when owners go out of scope
