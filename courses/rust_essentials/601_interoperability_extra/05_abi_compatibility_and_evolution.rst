=================================
ABI Compatibility and Evolution
=================================


-----------------------------------
What Becomes an External Contract
-----------------------------------

**Shipped boundary details become compatibility contracts**

* Exported symbol names
* Function signatures
* Struct sizes and field offsets
* Status-code meanings
* Ownership rules
* Required runtime libraries

.. note::

  Changing them can break foreign callers without any Rust compiler error


------------------
Evolving a C ABI
------------------

**Evolve a C ABI deliberately**

* Add a new exported symbol when a function signature changes
* Include :rust:`abi_version` and :rust:`struct_size` fields
* Reserve fields for future expansion
  * Define the values callers must use to initialize them
* Add capability-query functions for optional behavior
* Test compatibility against old headers and libraries

.. note::

  Cargo semantic versioning alone does not protect non-Cargo callers
