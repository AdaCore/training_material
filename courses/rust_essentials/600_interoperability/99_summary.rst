=========
Summary
=========

------------------------------
Recap: The Boundary Contract
------------------------------

**Every FFI boundary must make its agreements explicit**

* Calling convention and symbol names
* Data representation, layout, and validity
* Pointer validity, access, and ownership
* Error, panic, and exception behavior
* Callback registration, threads, concurrency, and reentrancy rules
* Linking, runtime, and compatibility requirements


-----------------
What We Covered
-----------------

* **C Foreign Function Interface**

  * :rust:`extern "C"`, exact boundary types, linking, and raw versus safe layers
  * Strings, opaque handles, status codes, ownership, and callbacks

* **Exporting Rust**

  * C ABI exports, caller contracts, allocator pairing, and ABI evolution

* **Binding Tools**

  * :rust:`bindgen` for raw declarations behind a reviewed safe wrapper

* **C++ Interoperability**

  * C-compatible facades and when to consider C++ bridge tools

* **Adoption and Migration Strategies**

  * Narrow boundaries, incremental adoption, and boundary redesign
