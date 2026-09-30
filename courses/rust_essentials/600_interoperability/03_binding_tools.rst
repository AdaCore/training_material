==============
Binding Tools
==============


-------------------
What bindgen Does
-------------------

:rust:`bindgen` **generates Rust declarations from C or C++ headers**

* **Common outputs**

  * Functions
  * Constants
  * Type aliases
  * Structs and unions
  * Selected layout tests

* **Good fit when**

  * Headers are large
  * Headers change regularly
  * Hand-maintained declarations are error-prone

.. note::

  Binding generation requires Clang/libclang


------------------------------
Recommended bindgen Workflow
------------------------------

**Keep generated bindings behind a reviewed safe wrapper**

.. image:: comprehensive_rust_training/600_bindgen_workflow.svg


-----------------------------
Managing Generated Bindings
-----------------------------

**Treat generated bindings as reviewed build artifacts**

* Allowlist only the needed API
* Pin the generation environment
* Generate deliberately rather than during every consumer build
* Review generated diffs
* Test layout and symbols on each supported platform
* Keep generated code out of the ergonomic public API


--------------------------
What bindgen Does Not Do
--------------------------

:rust:`bindgen` **generates declarations, not higher-level guarantees**

* **Memory contract**
  * Pointer validity, ownership, destruction, and callback lifetime
* **Concurrency contract**
  * Thread-safety requirements
* **Semantic contract**
  * Valid flags, C++ invariants, and failure policy
* **Runtime compatibility**
  * Loaded library matches the generated ABI
* **Boundary review** - still required

.. warning::

  Generated bindings are normally the raw layer, not the safe API
