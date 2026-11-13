================================
Exporting Rust Through a C ABI
================================


---------------------
Library Crate Types
---------------------

**Library crate types are specified in** :filename:`Cargo.toml`

.. code:: toml

  [lib]
  crate-type = ["staticlib"]  # or "cdylib"

* :rust:`staticlib` - Produces a static library for linking into a non-Rust program
* :rust:`cdylib` - Produces a dynamic library for use by non-Rust code

.. note::

  A :rust:`staticlib` may require additional platform libraries at final link time


-----------------------------
Exporting a Simple Function
-----------------------------

**C callers need a predictable symbol name for the exported function**

.. code:: rust

  use std::ffi::c_int;

  // SAFETY: `hyperdrive_check` is a unique symbol
  #[unsafe(no_mangle)]
  pub extern "C" fn hyperdrive_check(
      fuel: c_int, jumps: c_int,
  ) -> c_int {
      if fuel < 0 || jumps < 0 { return -1; }
      if jumps <= fuel / 10 { 1 } else { 0 }
  }

* Rust normally mangles symbol names; :rust:`extern "C"` does not disable this
* :rust:`#[unsafe(no_mangle)]` exports the exact symbol name :rust:`hyperdrive_check`
* The attribute is unsafe because the name must not collide with another linked symbol


-------------------------
C Header for the Export
-------------------------

**Foreign declarations must match the exported Rust signature**

.. code:: c

  #ifndef HYPERDRIVE_H
  #define HYPERDRIVE_H

  int hyperdrive_check(int fuel, int jumps);

  #endif

* Rust :rust:`c_int` matches C :C:`int`
* All :rust:`c_int` arguments are valid without extra safety preconditions
* Negative inputs return :rust:`-1`
* Other inputs return :rust:`0` or :rust:`1`


--------------------------------------
Calling the Exported Function from C
--------------------------------------

.. code:: c

  #include <stdio.h>
  #include "hyperdrive.h"

  int main(void) {
      int ready = hyperdrive_check(80, 6);
      printf("Hyperdrive ready: %d\n", ready);
      return 0;
  }

.. code:: output

  Hyperdrive ready: 1


----------------------------------
Pair Allocation and Deallocation
----------------------------------

**Allocation and deallocation must follow one ownership contract**

.. image:: rust_essentials/600_allocator_ownership.svg

.. note::

  Matching destructors or caller-owned buffers avoid allocator mismatch


-----------------------------
Returning an Owned C String
-----------------------------

:rust:`CString::into_raw` **transfers ownership across the C ABI**

.. code:: rust

  use std::ffi::{c_char, CString};

  // SAFETY: `droid_name_new` is unique
  #[unsafe(no_mangle)]
  pub extern "C" fn droid_name_new() -> *mut c_char {
      CString::new("R2-D2")
          .expect("literal contains no NUL")
          .into_raw()
  }


-----------------------------
Returned C String Ownership
-----------------------------

**The C caller is responsible for the returned string allocation**

* Preserve the string length and NUL terminator
* Release the allocation using :rust:`droid_name_free`
* Avoid accessing the allocation during deallocation
* Treat the pointer as invalid after releasing it

.. warning::

  Releasing the same pointer twice causes undefined behavior


------------------------------
Freeing an Exported C String
------------------------------

:rust:`CString::from_raw` **reclaims ownership of the Rust-allocated string**

.. code:: rust

  /// # Safety
  /// `ptr` is null or uniquely owned, from `droid_name_new`,
  /// and still has its original string length
  // SAFETY: `droid_name_free` is unique
  #[unsafe(no_mangle)]
  pub unsafe extern "C" fn droid_name_free(
      ptr: *mut c_char,
  ) {
      if ptr.is_null() { return; }
      // SAFETY: The caller upholds the ownership contract
      drop(unsafe { CString::from_raw(ptr) });
  }

.. note::

  Dropping :rust:`CString` frees its allocation through Rust's allocator
