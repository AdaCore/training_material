==============
Introduction
==============


----------------
Topics Covered
----------------


* **Why Interoperability Matters**


  * Add Rust without rewriting an entire system


* **C Foreign Function Interface**


  * ABI, :rust:`extern "C"`, linking, and C-compatible data


* **Safe Boundary Design**


  * Raw declarations behind safe Rust interfaces


* **Calling and Exporting**


  * Rust calls C; C calls Rust


* **Binding Tools**


  * Where :rust:`bindgen` helps and where it does not


* **C++ Interoperability**


  * Recommended bridge strategies and current limitations


* **Adoption and Migration Strategies**


  * Incremental, mixed-language, replacement, and greenfield


------------------------------
Why Interoperability Matters
------------------------------

**Existing systems contain more than source code**

* Proven algorithms
* Hardware and operating-system integrations
* Certification evidence
* Mature test suites
* Vendor libraries
* Years of operational knowledge
* Interoperability lets Rust be added without discarding those assets

.. note::

  Migration can be incremental rather than all-or-nothing


--------------------------------
Interoperability Is a Contract
--------------------------------

**Interoperability requires a complete boundary contract**

* **Calling convention** - how arguments and return values are passed
* **Symbol name** - what the linker searches for
* **Data layout** - size, alignment, and field offsets
* **Validity** - which bit patterns and pointer values are allowed
* **Ownership** - who allocates, mutates, and frees
* **Control flow** - how errors, panics, and exceptions behave
* **Concurrency** - which threads may call or receive callbacks


----------------------------
From Source Code to a Call
----------------------------

**A foreign call spans build-time and runtime steps**

.. image:: rust_essentials/600_ffi_source_to_call.svg

* Rust compiler checks the Rust declaration
* It cannot prove that the foreign implementation matches it


--------------------------
Choose a Narrow Boundary
--------------------------

**Good boundaries are narrow and explicit**

* Coarse-grained rather than one call per field access
* Based on simple C-compatible data
* Explicit about ownership
* Small enough to review and test
* Stable enough to version
* Wrapped once rather than used directly throughout the crate
* Avoid mirroring an entire data model across the boundary

.. tip::

  Move complete operations across the boundary, not chatty calls
