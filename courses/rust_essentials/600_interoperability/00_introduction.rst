==============
Introduction
==============


----------------
Topics Covered
----------------


* **Calling C from Rust**

  * C ABI (application binary interface), foreign declarations, calls, and linking

* **Safe FFI Boundaries**

  * Raw and safe layers
  * Pointer validity and contracts
  * C-compatible data representations

* **C Strings Across the Boundary**

  * C string representations, validity, and borrowing

* **Callbacks, Ownership, and Errors**

  * Callback contracts, handles, ownership, status codes, and unwinding

* **Exporting Rust**

  * C ABI exports and their caller contracts


------------------------------
Why Interoperability Matters
------------------------------

**Interoperability connects new code to existing systems**

* **For developers**
  * Reuse proven algorithms and vendor libraries
  * Integrate with hardware and OS interfaces
  * Extend systems without rewriting existing components

* **For organizations**
  * Preserve mature test suites and operational knowledge
  * Keep existing certification evidence available for review


--------------------------------
Interoperability Is a Contract
--------------------------------

**Interoperability requires a complete boundary contract**

.. list-table::
   :header-rows: 1
   :widths: 32 68

   * - **Contract Element**
     - **Description**
   * - *Calling Convention*
     - How arguments and return values are passed
   * - *Symbol Name*
     - What the linker searches for
   * - *Data Layout*
     - Size, alignment, and field offsets
   * - *Validity*
     - Allowed bit patterns and pointer values
   * - *Ownership*
     - Who allocates, mutates, and frees
   * - *Error Handling*
     - How errors are represented and unwinding is contained
   * - *Concurrency*
     - Which threads may invoke or receive callbacks


----------------------------
From Source Code to a Call
----------------------------

.. image:: rust_essentials/600_ffi_source_to_call.svg

* A Rust declaration describes the expected native function interface

.. warning::

  Rust cannot prove that the implementation matches the declaration
