=================================
C++ Interoperability (Optional)
=================================


----------------------------------
Default C++ Strategy: A C Facade
----------------------------------

**Use a C-compatible facade as the portable default**

.. image:: comprehensive_rust_training/600_cpp_c_facade.svg


---------------------------
Optional C++ Bridge Tools
---------------------------

**Use a C++ bridge only when both sides can adopt and qualify it**

* Code generation is acceptable
* The supported shared type set is sufficient
* The project can pin and qualify the tool
* **Parsing is not enough**
  * :rust:`bindgen` parses declarations; wrappers handle C++ semantics
