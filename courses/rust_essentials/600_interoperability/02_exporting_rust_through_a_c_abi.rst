================================
Exporting Rust Through a C ABI
================================


---------------------
Library Crate Types
---------------------

**Choose the crate type for the foreign link model**

.. code:: toml

  [lib]
  crate-type = ["staticlib"]

* **Static system library**

  * :rust:`staticlib` for a non-Rust executable

* **Dynamic system library**

  * :rust:`cdylib` loaded by another language

* **Rust compiler library**

  * :rust:`rlib` is not a general foreign-language boundary

.. note::

  The foreign linker may also need Rust platform libraries


-----------------------------
Exporting a Simple Function
-----------------------------

**Rust 2024 writes** :rust:`no_mangle` **as an unsafe attribute**

.. code:: rust

  use std::ffi::c_int;

  // SAFETY: This crate defines the only exported symbol named
  // `hyperdrive_check` in the final linked program
  #[unsafe(no_mangle)]
  pub extern "C" fn hyperdrive_check(
      fuel: c_int,
      jumps: c_int,
  ) -> c_int {
      if fuel < 0 || jumps < 0 {
          return -1;
      }

      if jumps <= fuel / 10 { 1 } else { 0 }
  }

* Scalar arguments have no caller safety preconditions
* Invalid values are returned explicitly


-------------------------
C Header for the Export
-------------------------

**The foreign declaration mirrors the exported Rust signature**

.. code:: c

  #ifndef HYPERDRIVE_H
  #define HYPERDRIVE_H

  int hyperdrive_check(int fuel, int jumps);

  #endif


--------------------------------------
Calling the Exported Function from C
--------------------------------------

**The C caller uses the generated or reviewed header**

.. code:: c

  #include <stdio.h>
  #include "hyperdrive.h"

  int main(void) {
      int ready = hyperdrive_check(80, 6);
      printf("Hyperdrive ready: %d\n", ready);
      return 0;
  }

:command:`Hyperdrive ready: 1`


--------------------------
Buffer Boundary Contract
--------------------------

**Unsafe exported buffers need an explicit caller contract**

* **Readable input** - :rust:`values` has :rust:`length` readable :rust:`i32` values
  * Required only when :rust:`length > 0`
* **Valid range** - aligned, one allocation, and non-wrapping
* **Range bound** - at most :rust:`isize::MAX` bytes and unmodified
* **Output** - :rust:`out_total` is aligned, writable, and non-overlapping
* **Runtime checks** - only null pointers are rejected


----------------------------
Validating Buffer Pointers
----------------------------

**Reject invalid null-pointer cases before creating Rust references**

.. code:: rust

  #[unsafe(no_mangle)]
  pub unsafe extern "C" fn crew_total(
      values: *const i32,
      length: usize,
      out_total: *mut i32,
  ) -> i32 {
      if out_total.is_null()
          || (length > 0 && values.is_null())
      {
          return 1;
      }

      // Continue with the validated pointers
      todo!()
  }

* :rust:`out_total` must always be non-null
* :rust:`values` may be null only when :rust:`length == 0`


-------------------------
Creating the Rust Slice
-------------------------

**After validation, the raw input can be viewed as a Rust slice**

.. code:: rust

  use std::slice;

  let crew: &[i32] = if length == 0 {
      &[]
  } else {
      // SAFETY: Required by the documented buffer contract
      unsafe { slice::from_raw_parts(values, length) }
  };

  let total = crew.iter().copied().try_fold(0_i32, i32::checked_add);
  let Some(total) = total else { return 2; };

  // SAFETY: Output is writable and does not overlap the input
  unsafe { *out_total = total };
  0


--------------------------------
Use the Same Allocator to Free
--------------------------------

**Memory should be released by the runtime that allocated it**

.. image:: comprehensive_rust_training/600_allocator_ownership.svg

.. note::

  Matching destructors or caller-owned buffers avoid allocator mismatch


-----------------------------
Returning an Owned C String
-----------------------------

:rust:`CString::into_raw` **transfers ownership to the caller**

.. code:: rust

  use std::ffi::{c_char, CString};

  // SAFETY: This crate defines the only exported symbol named
  // `droid_name_new` in the final linked program
  #[unsafe(no_mangle)]
  pub extern "C" fn droid_name_new() -> *mut c_char {
      CString::new("R2-D2")
          .expect("literal contains no NUL")
          .into_raw()
  }

* **Caller ownership** - the returned pointer belongs to foreign code
* **Release path** - return it through :rust:`droid_name_free`

.. warning::

  Do not call C :C:`free` on a pointer returned by :rust:`CString::into_raw`


------------------------------
Freeing an Exported C String
------------------------------

:rust:`CString::from_raw` **retakes ownership from the caller**

* **Caller contract**
  * :rust:`ptr` is null or a live pointer returned by :rust:`droid_name_new`
  * Foreign code has not changed the string length
  * No other pointer accesses the allocation during this call

.. code:: rust

  #[unsafe(no_mangle)]
  pub unsafe extern "C" fn droid_name_free(ptr: *mut c_char) {
      if ptr.is_null() { return; }

      // SAFETY: Required by the caller contract
      drop(unsafe { CString::from_raw(ptr) });
  }

* :rust:`from_raw` reconstructs the original Rust-owned :rust:`CString`
* Dropping it uses the matching Rust allocator
* **Ownership rule** - never reuse or free the pointer twice


------------------------------
What Becomes Part of the ABI
------------------------------

**Shipped ABI details become external contracts**

* Exported symbol names
* Function signatures
* Struct sizes and field offsets
* Status-code meanings
* Ownership rules
* Required runtime libraries

.. note::

  ABI changes can break foreign callers even when Rust still builds


------------------
Evolving a C ABI
------------------

**Evolve a C ABI deliberately**

* Version exported function names when signatures change
* Include :rust:`abi_version` and :rust:`struct_size` fields
* Reserve fields and define their required initialization
* Add capability-query functions for optional behavior
* Test compatibility against old headers and libraries
* Cargo semantic versioning alone does not protect non-Cargo callers
