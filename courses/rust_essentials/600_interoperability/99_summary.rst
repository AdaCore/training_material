=========
Summary
=========

-----------------
What We Covered
-----------------

**FFI requires explicit ABI, data, and behavior contracts**

* **Calling C from Rust**

  - :rust:`extern "C"`, C types, symbol names, linking, and runtime compatibility

* **Safe Boundary Design**

  - Review data layout; enforce pointer validity, access, and ownership rules
  - Specify string and handle lifetimes, callback threading and reentrancy
  - Map errors to values; contain panics and foreign exceptions

* **Binding Tools**

  - :rust:`bindgen` produces raw declarations needing reviewed safe wrappers

* **Exporting Rust**

  - Document caller contracts and pair allocations with matching destructors
