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


---------------------------
Buffer Function Signature
---------------------------

**The C API passes an input pointer/length pair and an output pointer**

.. code:: c

  #include <stddef.h>
  #include <stdint.h>

  // C callers must meet both buffer contracts
  int32_t crew_total_checked(
      const int32_t *values,
      size_t length,
      int32_t *out_total
  );

* Status :C:`0`: success; :C:`1`: invalid null pointer; :C:`2`: overflow


--------------------------
Buffer Boundary Contract
--------------------------

**Callers supply a valid input range, not just a non-null pointer**

* When :rust:`length > 0`, :rust:`values` names that many initialized :rust:`i32` values
* Input is readable, aligned, and within a single non-wrapping allocation
* Input size is at most :rust:`isize::MAX` bytes
* Foreign code cannot modify the input during the call
* :rust:`values` may be null when :rust:`length == 0`


------------------------
Output Buffer Contract
------------------------

**The output pointer has a separate caller contract**

* :rust:`out_total` points to one aligned, writable :rust:`i32`
* Its storage must not overlap the input range
* The function writes the output only on success
* Runtime checks reject null pointers only
  * They cannot establish alignment, allocation bounds, or ownership


----------------------------------
Declaring the Rust Buffer Export
----------------------------------

**Signature fragment; the implementation follows on the next slides**

.. code:: rust

  /// # Safety
  /// The caller upholds the input and output buffer contracts
  // SAFETY: `crew_total_checked` is unique
  #[unsafe(no_mangle)]
  pub unsafe extern "C" fn crew_total_checked(
      values: *const i32,
      length: usize,
      out_total: *mut i32,
  ) -> i32 {
      // Body shown on the following slides
      // ...
  }


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

* Rust callers must uphold the :rust:`unsafe fn` safety contract
* C callers must follow the documented C API contract
* :rust:`out_total` must always be non-null
* :rust:`values` may be null only when :rust:`length == 0`


-------------------------
Creating the Rust Slice
-------------------------

**Construct a borrowed slice only after checking null pointers**

.. code:: rust

  // Continue inside `crew_total_checked`
  let crew: &[i32] = if length == 0 {
      &[]
  } else {
      // SAFETY: The caller guarantees one valid allocation
      // of `length` aligned, initialized, stable values
      unsafe {
          core::slice::from_raw_parts(values, length)
      }
  };

* The empty case avoids constructing a slice from a null pointer


--------------------------
Summing Without Overflow
--------------------------

**Check each addition before using its result**

.. code:: rust

  // Continue inside `crew_total_checked`
  let mut total = 0_i32;
  for value in crew {
      let Some(next) = total.checked_add(*value) else {
          return 2;
      };
      total = next;
  }

* Overflow returns status :rust:`2` without writing the output


--------------------
Writing the Output
--------------------

**Write the result only after every addition succeeds**

.. code:: rust

  // Final statements inside `crew_total_checked`
  // SAFETY: The caller provides writable storage
  // that does not overlap the input range
  unsafe { *out_total = total };
  0

* The return value :rust:`0` reports success


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
