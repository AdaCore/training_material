=========
Summary
=========

------------------------------
Recap: The Boundary Contract
------------------------------

**Every FFI boundary must make its agreements explicit**

* Calling convention and symbol names
* Data representation, layout, and validity
* Pointer lifetime, access, and ownership
* Error, panic, and exception behavior
* Callback thread, concurrency, and reentrancy rules
* Linking, runtime, and compatibility requirements


-----------------
What We Covered
-----------------

* **C Foreign Function Interface**

  * :rust:`extern "C"`, exact boundary types, linking, and raw versus safe layers

* **Strings, Handles, and Errors**

  * Explicit ownership, matching destructors, and typed :rust:`Result`

* **Exporting Rust**

  * C ABI exports, caller contracts, allocator pairing, and ABI evolution

* **Binding and C++**

  * :rust:`bindgen` for raw declarations and a C facade for portable C++ interop

* **Migration Strategies**

  * Narrow boundaries, incremental adoption, and boundary redesign
