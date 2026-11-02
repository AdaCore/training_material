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
* Callback lifetimes, threading, concurrency, and reentrancy rules
* Linking, runtime, and compatibility requirements


-----------------
What We Covered
-----------------

* **Calling C from Rust**

  * :rust:`extern "C"`, C ABI types, function calls, and linking

* **Safe Boundary Design**

  * Raw declarations behind safe wrappers, pointer and layout contracts
  * Strings, callback contracts, ownership, opaque handles, errors, and unwinding

* **Binding Tools**

  * :rust:`bindgen` for raw declarations behind a reviewed safe wrapper

* **Exporting Rust**

  * C ABI exports, caller contracts, and allocator pairing
