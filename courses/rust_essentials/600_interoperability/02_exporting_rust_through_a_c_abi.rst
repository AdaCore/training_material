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
* Rust library
  * :rust:`rlib` is not a general foreign-language boundary

.. note::

  A :rust:`staticlib` may require additional platform libraries at final link time


-----------------------------
Exporting a Simple Function
-----------------------------

**In Rust 2024,** :rust:`no_mangle` **is an unsafe attribute**

.. code:: rust

  use std::ffi::c_int;

  // SAFETY: `hyperdrive_check` does not collide
  // with any other linked symbol
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

* Calling this function has no safety preconditions on the scalar values
* Invalid inputs are reported with an explicit return value


-------------------------
C Header for the Export
-------------------------

**The foreign declaration mirrors the exported Rust signature**

.. code:: c

  #ifndef HYPERDRIVE_H
  #define HYPERDRIVE_H

  #ifdef __cplusplus
  extern "C" {
  #endif

  int hyperdrive_check(int fuel, int jumps);

  #ifdef __cplusplus
  }
  #endif

  #endif

* Rust :rust:`c_int` matches C :C:`int`
* C++ guards preserve C linkage when the header is included from C++


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


--------------------------
Buffer Boundary Contract
--------------------------

**The exported buffer function needs an explicit caller contract**

.. code:: c

  #include <stddef.h>
  #include <stdint.h>

  int32_t crew_total_checked(
      const int32_t *values,
      size_t length,
      int32_t *out_total
  );

* Readable input
  * :rust:`values` provides :rust:`length` readable :rust:`i32` values when :rust:`length > 0`
* Valid range
  * Input range is aligned, within one allocation, and non-wrapping
* Range bound
  * At most :rust:`isize::MAX` bytes and unmodified during the call
* Output
  * :rust:`out_total` points to aligned writable :rust:`i32` storage
  * Does not overlap the input range
  * Written only on success
* Pointer checks
  * Only null-pointer cases are rejected at runtime


------------------------
Checking Null Pointers
------------------------

**Rust export is unsafe because callers must uphold the buffer contract**

.. code:: rust

  // Inside `crew_total_checked`
  if out_total.is_null()
      || (length > 0 && values.is_null())
  {
      return 1;
  }

* :rust:`unsafe` makes the documented pointer contract a caller obligation
* :rust:`out_total` must always be non-null
* :rust:`values` may be null only when :rust:`length == 0`


-------------------------
Creating the Rust Slice
-------------------------

**Continue inside** :rust:`crew_total_checked` **after the null checks**

.. code:: rust

  let crew: &[i32] = if length == 0 {
      &[]
  } else {
      // SAFETY: Required by the documented buffer contract
      unsafe { std::slice::from_raw_parts(values, length) }
  };

  let mut total = 0_i32;
  for value in crew {
      let Some(next) = total.checked_add(*value) else {
          return 2;
      };
      total = next;
  }

  // SAFETY: Output is writable and does not overlap the input
  unsafe { *out_total = total };
  0


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

:rust:`CString::into_raw` **transfers ownership to the caller**

.. code:: rust

  use std::ffi::{c_char, CString};

  // SAFETY: `droid_name_new` does not collide
  // with any other linked symbol
  #[unsafe(no_mangle)]
  pub extern "C" fn droid_name_new() -> *mut c_char {
      CString::new("R2-D2")
          .expect("literal contains no NUL")
          .into_raw()
  }

* Caller ownership
  * Returned pointer belongs to foreign code
* Release path
  * Return it through :rust:`droid_name_free`

.. warning::

  Release the pointer only through :rust:`droid_name_free`


------------------------------
Freeing an Exported C String
------------------------------

:rust:`CString::from_raw` **retakes ownership from the caller**

* Caller contract
  * :rust:`ptr` is null or a live pointer returned by :rust:`droid_name_new`
  * Foreign code has not changed the string length
  * No other pointer accesses the allocation during this call

.. code:: rust

  // SAFETY: `droid_name_free` does not collide
  // with any other linked symbol
  #[unsafe(no_mangle)]
  pub unsafe extern "C" fn droid_name_free(ptr: *mut c_char) {
      if ptr.is_null() { return; }

      // SAFETY: Required by the caller contract
      drop(unsafe { CString::from_raw(ptr) });
  }

* :rust:`from_raw` reconstructs the original Rust-owned :rust:`CString`
* Dropping it uses the matching Rust allocator
* A non-null :rust:`ptr` is no longer valid after the call


-----------------------------------
What Becomes an External Contract
-----------------------------------

**Shipped boundary details become compatibility contracts**

* Exported symbol names
* Function signatures
* Struct sizes and field offsets
* Status-code meanings
* Ownership rules
* Required runtime libraries

.. note::

  Changing them can break foreign callers without any Rust compiler error


------------------
Evolving a C ABI
------------------

**Evolve a C ABI deliberately**

* Add a new exported symbol when a function signature changes
* Include :rust:`abi_version` and :rust:`struct_size` fields
* Reserve fields for future expansion
  * Define the values callers must use to initialize them
* Add capability-query functions for optional behavior
* Test compatibility against old headers and libraries

.. note::

  Cargo semantic versioning alone does not protect non-Cargo callers
