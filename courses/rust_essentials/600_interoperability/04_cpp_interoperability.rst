======================
C++ Interoperability
======================


----------------------------------
Default C++ Strategy: A C Facade
----------------------------------

**Use a C-compatible facade as the portable default**

.. image:: rust_essentials/600_cpp_c_facade.svg


-----------------------------------
When to Consider C++ Bridge Tools
-----------------------------------

**Consider a C++ bridge only when both sides can adopt and qualify it**

* Adoption conditions
  * Code generation is acceptable
  * Supported shared type set is sufficient
  * Project can pin and qualify the tool
* Parsing is not enough
  * :rust:`bindgen` parses declarations; wrappers handle C++ semantics
