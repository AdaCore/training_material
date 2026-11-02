================================
Exporting Rust Through a C ABI
================================


---------------------
Library Crate Types
---------------------

**Choose the crate type for the foreign link model**

.. code:: toml

  [lib]
  crate-type = ["staticlib"]  # or "cdylib"

* Static system library
  * :rust:`staticlib` for linking into a non-Rust program
* Dynamic system library
  * :rust:`cdylib` for dynamic linking from non-Rust code

.. note::

  A :rust:`staticlib` may require additional platform libraries at final link time


-----------------------------
Exporting a Simple Function
-----------------------------

**Export a C ABI symbol without name mangling**

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


-------------------------
C Header for the Export
-------------------------

**The foreign declaration mirrors the exported Rust signature**

.. code:: c

  #ifndef HYPERDRIVE_H
  #define HYPERDRIVE_H

  int hyperdrive_check(int fuel, int jumps);

  #endif

* Rust :rust:`c_int` matches C :C:`int`
* All :rust:`c_int` arguments are valid without extra safety preconditions
* Negative inputs return :rust:`-1`; other inputs return :rust:`0` or :rust:`1`
* C callers must uphold buffer and ownership rules beyond null checks


--------------------------------------
Calling the Exported Function from C
--------------------------------------

**The C caller includes the matching header**

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


----------------------------
Returned CString Ownership
----------------------------

**The caller owns the allocation but must use the matching destructor**

* Pass the pointer back to :rust:`droid_name_free` for deallocation
* Do not call C :C:`free()` on the Rust-allocated string
* Do not change the string length or its NUL terminator
* Do not access the allocation while the destructor runs
* Stop using the pointer after returning it to Rust

.. warning::

  Release the returned pointer only once


------------------------------
Freeing an Exported C String
------------------------------

:rust:`CString::from_raw` **reconstructs the original allocation**

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

* Dropping it uses the matching Rust allocator
