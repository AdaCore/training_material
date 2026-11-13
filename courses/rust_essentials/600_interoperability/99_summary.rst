=========
Summary
=========

-----------------
What We Covered
-----------------

* **Calling C from Rust**

  - Foreign declarations describe the ABI contract for calling C
  - Linking resolves symbol references to their implementations

* **Safe FFI Boundaries**

  - Safe wrappers depend on explicit pointer and ownership contracts
  - :rust:`#[repr(C)]` provides C-compatible struct layout

* **C Strings Across the Boundary**

  - Borrowed C strings require valid NUL-terminated storage
  - UTF-8 conversion and ownership are handled explicitly

* **Callbacks, Ownership, and Errors**

  - Callback and handle contracts define valid usage and lifetimes
  - C status codes can be mapped to Rust :rust:`Result`
  - Panics and foreign exceptions must not unwind across :rust:`extern "C"` boundaries

* **Exporting Rust**

  - Rust-owned allocations require matching Rust destructors
