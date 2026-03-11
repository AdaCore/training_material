============
Approaches
============

---------------------------------
Approaches to Memory Management
---------------------------------

- **Manual Memory Management** (e.g., C/C++)
  - Full Control: Programmer explicitly allocates/deallocates
  - Higher Risk: Programmer must ensure pointers are valid
- **Automatic Memory Management** (e.g., Java, Python)
  - Safety: GC ensures memory is not freed until unreferenced
  - Higher Cost: Runtime overhead for garbage collection
- **Ownership-based Memory Management** (e.g., Rust, SPARK)
  - Control & Safety: Compile-time memory guarantees
  - Mechanism: Ownership, borrowing and lifetimes

-----------------------
Navigating Approaches
-----------------------

.. container:: latex_environment scriptsize

  .. list-table::
    :header-rows: 1

    * - **Feature**
      - **Manual (C/C++)**
      - **Automatic (Java/Python)**
      - **Ownership (Rust, SPARK)**

    * - *Control*
      - Full
      - Low
      - High

    * - *Safety*
      - High risk of error
      - High safety
      - Compile-time safety

    * - *Mechanism*
      - :cpp:`malloc`/:cpp:`free`
      - Garbage collector
      - Ownership & drop

    * - *Overhead*
      - Minimal
      - Runtime slowdown
      - Zero
