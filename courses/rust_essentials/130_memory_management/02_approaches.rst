============
Approaches
============

---------------------------------
Approaches to Memory Management
---------------------------------

- **Manual Memory Management** (e.g., C/C++)
  - *Full control*
    - Programmer explicitly allocates/deallocates
  - *Higher risk*
    - Programmer must ensure pointers are valid
- **Automatic Memory Management** (e.g., Java, Python)
  - *Safety*
    - Runtime ensures memory is not freed until unreferenced
  - *Higher cost*
    - Runtime overhead for garbage collection
- **Ownership-based Memory Management** (e.g., Rust, SPARK)
  - *Safety*
    - Compile-time memory guarantees
  - *Control*
    - Ownership, borrowing and lifetimes

------------------
Quick Comparison
------------------

.. container:: latex_environment tiny

  .. list-table::
    :header-rows: 1
    :stub-columns: 1

    * - **Feature**
      - **Manual (C/C++)**
      - **Automatic (Java, Python)**
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
      - Borrow checker

    * - *Runtime Overhead*
      - Minimal
      - *Stop-the-World* pauses
      - Zero

    * - *Developer Overhead*
      - Manual tracking
      - Low
      - Compilation time

.. note::

    Rust offers memory safety, predictable performance, and zero runtime cost 
