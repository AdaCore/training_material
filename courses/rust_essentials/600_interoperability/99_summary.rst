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

* **Calling C from Rust**

  * :rust:`extern "C"`, C ABI types, function calls, linking, and build scripts

* **Safe Boundary Design**

  * Raw declarations behind safe wrappers, pointer and layout contracts
  * Strings, callbacks, ownership, opaque handles, errors, and unwinding

* **Binding Tools**

  * :rust:`bindgen` for raw declarations behind a reviewed safe wrapper

* **Exporting Rust**

  * C ABI exports, caller contracts, allocator pairing, and ABI evolution

* **C++ Interoperability**

  * C-compatible facades and when to consider C++ bridge tools

* **Adoption and Migration Strategies**

  * Narrow boundaries, incremental adoption, and boundary redesign
