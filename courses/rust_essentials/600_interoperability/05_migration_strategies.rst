===================================
Adoption and Migration Strategies
===================================


----------------------------
Four Common Adoption Paths
----------------------------

**Rust adoption path depends on system constraints and boundary cost**

* Incremental adoption
  * Replace one stable component at a time
* Long-term mixed-language
  * Keep selected existing components behind explicit boundaries
* Replace over time
  * Keep interfaces stable while implementations move to Rust
* Greenfield Rust
  * Start in Rust and use FFI for required native dependencies


----------------------
Incremental Adoption
----------------------

* Incremental adoption works best around a narrow boundary

#. Select a component with few dependencies and a narrow interface
#. Capture current behavior with tests
#. Define a C-compatible boundary
#. Implement the replacement in Rust
#. Run old and new implementations against the same tests
#. Deploy behind a feature or configuration switch
#. Remove the old implementation only after evidence is sufficient

* Good early candidates
  * Clear inputs and outputs
  * Limited shared mutable state
  * Few callbacks
  * High value from memory safety


-----------------------
Branch by Abstraction
-----------------------

**Keep a stable abstraction while implementation moves gradually**

.. image:: rust_essentials/600_strangler_pattern.svg
   :width: 100%
   :align: center

.. note::

  Supports rollback and side-by-side comparison


-------------------------------------
Signs the Boundary Is Too Expensive
-------------------------------------

**Boundary cost often appears as a combination of warning signs**

* Chatty calls
  * Many tiny calls or frequent callbacks
* Heavy data movement
  * Large amounts of data copied repeatedly
* Coupled data models
  * Shared ownership or unstable layouts
* Runtime mismatch
  * Different threading, allocator, or failure models
* Boundary shape
  * Often the real problem, not FFI itself


-------------------------------
Move or Redesign the Boundary
-------------------------------

**When boundary cost dominates, change the interface shape**

* Move the boundary outward around a larger operation
* Batch fine-grained calls or data transfers
* Transfer serialized messages
* Replace a larger component
* Keep the component in its original language

.. tip::

  Prefer fewer, coarser, and more explicit cross-language interactions


-----------------
Greenfield Rust
-----------------

**Greenfield Rust can still need FFI dependencies**

* Common reasons
  * Operating-system APIs
  * Device drivers
  * Vendor SDKs
  * Cryptographic libraries
  * Graphics or media libraries
  * Existing C/C++ platform services
* Questions before choosing FFI
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
* Pointer relationships
  * Nullability and pointer/length relationships
  * Read/write permissions
* String encoding


---------------------------------
Review Behavior and Integration
---------------------------------

**Verify the behavioral and integration contract**

* Ownership and destruction
* Error mapping
* Panic and exception policy
* Callback contract
  * Registration duration
  * Calling threads, concurrency, and reentrancy
* Static/dynamic linking requirements
* Compatibility and regression tests
* Review both language declarations side by side
