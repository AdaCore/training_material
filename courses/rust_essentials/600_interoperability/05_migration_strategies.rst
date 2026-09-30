======================
Migration Strategies
======================


------------------------
Four Common Strategies
------------------------

**Migration strategy depends on system constraints and boundary cost**

* **Incremental adoption**
  * Replace one stable component
* **Long-term mixed-language**
  * Retain valuable components
* **Replace over time**
  * Keep interfaces stable while internals change
* **Greenfield Rust**
  * No legacy compatibility boundary


----------------------
Incremental Adoption
----------------------

**Incremental adoption works best around a narrow boundary**

#. Select a leaf component with a narrow interface
#. Record current behavior with tests
#. Define a C-compatible boundary
#. Implement the replacement in Rust
#. Run old and new implementations against the same tests
#. Deploy behind a feature or configuration switch
#. Remove the old implementation only after evidence is sufficient

* **Good early candidates**
  * Clear inputs and outputs
  * Limited shared mutable state
  * Few callbacks
  * High value from memory safety


-----------------------
The Strangler Pattern
-----------------------

**Keep the boundary stable while implementation moves gradually**

.. image:: comprehensive_rust_training/600_strangler_pattern.svg

.. note::

  Supports rollback and side-by-side comparison


-------------------------------------
Signs the Boundary Is Too Expensive
-------------------------------------

**Boundary cost often appears as a combination of warning signs**

* **Chatty calls** - many tiny calls or frequent callbacks
* **Heavy data movement** - large amounts of data copied repeatedly
* **Coupled data models** - shared ownership or unstable layouts
* **Runtime mismatch** - different threading, allocator, or failure models
* **Boundary shape** - often the real problem, not FFI itself


-------------------------------
Move or Redesign the Boundary
-------------------------------

**When boundary cost dominates, change the interface shape**

* Move the boundary outward around a larger operation
* Batch fine-grained calls or data transfers
* Transfer serialized messages
* Replace a larger component
* Keep the component in its original language
* Aim for fewer, coarser, and more explicit cross-language interactions


-----------------
Greenfield Rust
-----------------

**Greenfield Rust can still need FFI dependencies**

* **Common reasons**
  * Operating-system APIs
  * Device drivers
  * Vendor SDKs
  * Cryptographic libraries
  * Graphics or media libraries
  * Existing C/C++ platform services
* **Questions before choosing FFI**
  * Is a maintained safe Rust crate already available?
  * Is the native dependency part of the product strategy?
  * Can its build and ABI be reproduced on every supported platform?
  * Who owns future binding maintenance?


----------------------------------
Review the ABI and Data Contract
----------------------------------

**Verify the binary and data-shape contract**

* ABI and symbol names
* Exact boundary types
* Struct layout and alignment
* Nullability and pointer length
* Read/write permissions
* String encoding


---------------------------------
Review Lifetime and Integration
---------------------------------

**Verify the behavioral and integration contract**

* Ownership and destruction
* Error mapping
* Panic and exception policy
* Callback lifetime and threads
* Static/dynamic linking requirements
* Compatibility and regression tests
* **Cross-check** - review both language declarations side by side
