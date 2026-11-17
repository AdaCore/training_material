=========================
Advanced Buffer Exports
=========================


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

* When :rust:`length > 0` and :rust:`values` is non-null, it names that many initialized :rust:`i32` elements
* Input is readable, aligned, and within a single non-wrapping allocation
* Input size is at most :rust:`isize::MAX` bytes
* Foreign code cannot modify the input during the call
* :rust:`values` may be null when :rust:`length == 0`


------------------------
Output Buffer Contract
------------------------

**The output pointer has a separate caller contract**

* A non-null :rust:`out_total` points to one aligned, writable :rust:`i32`
* Null :rust:`out_total` returns status :rust:`1`
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
  /// Non-null pointers must satisfy their buffer contracts
  /// Input and output storage must not overlap
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
* Null :rust:`out_total` returns status :rust:`1`
* Null :rust:`values` with :rust:`length > 0` returns status :rust:`1`


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
