=========
Summary
=========

-------------------------
Recap: Memory Semantics
-------------------------

.. list-table::
  :header-rows: 1

  * - **Mechanism**
    - **Behavior**
    - **Impact**
    - **Trigger**

  * - *Move*
    - Ownership transfer
    - Zero
    - Default for non-:rust:`Copy` types

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
  
- **Move**
  - Transfers ownership
- **Copy**
  - Implicit bitwise copy
- **Clone**
  - Explicit deep copy, required for non-:rust:`Copy` types

- **Clean-up**
  - Resources are freed the moment owner exits its scope
  - :rust:`Drop` trait allows custom destructor logic
  - Manually triggered destruction with :rust:`std::mem::drop`
  