===============
Binding Tools
===============


---------------------
What "bindgen" Does
---------------------

:rust:`bindgen` **generates Rust declarations from C or C++ headers**

* Common outputs
  * Functions
  * Constants
  * Type aliases
  * Structs and unions
  * Selected layout tests
* Good fit when
  * Headers are large
  * Headers change regularly
  * Hand-maintained declarations are error-prone

.. note::

  Only generation requires Clang/libclang; pre-generated bindings do not


------------------------------
Recommended bindgen Workflow
------------------------------

**Keep generated bindings behind a reviewed safe wrapper**

.. image:: rust_essentials/600_bindgen_workflow.svg


-----------------------------
Managing Generated Bindings
-----------------------------

**Treat generated bindings as reviewed generated code**

* Allowlist only the needed API
* Pin :rust:`bindgen`, Clang/libclang, and header versions
* Generate deliberately rather than during every consumer build
* Review generated diffs
* Test layout and symbols on each supported platform
* Keep generated code out of the ergonomic public API


----------------------------
What "bindgen" Does Not Do
----------------------------

:rust:`bindgen` **generates declarations, not higher-level guarantees**

* Memory contract
  * Pointer validity, ownership, and destruction
  * Callback and context validity duration
* Concurrency contract
  * Thread-safety requirements
* Semantic contract
  * Valid flags, C++ invariants, and failure policy
* Binary compatibility
  * Linked or loaded library matches the generated ABI
* Boundary review
  * Still required

.. warning::

  Generated bindings are normally the raw layer, not the safe API
