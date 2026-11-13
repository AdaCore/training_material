=============================
C Strings Across the Boundary
=============================


--------------------------------
C Strings Are Not Rust Strings
--------------------------------

**C strings and Rust strings have different representations**

* **C string**
  * NUL-terminated sequence of bytes, usually accessed through a pointer
  * Not automatically UTF-8
  * Valid only while its backing storage exists
* :rust:`String`
  * Owned and growable
  * UTF-8
  * Rust-specific internal representation
* Examples use :rust:`std` for convenience
  * :rust:`core` supports borrowed C strings
  * :rust:`alloc` supports owned C strings when an allocator is available

.. warning::

  Do not pass :rust:`String` or :rust:`&str` directly through a C ABI


-------------------------------
Receiving a Borrowed C String
-------------------------------

**The caller must establish the pointer contract before a borrowed C string is read**

* Null means no C string
* Otherwise, the pointer names initialized, readable NUL-terminated bytes
* The entire range, including NUL, is within one allocation and :rust:`isize::MAX` bytes
* The bytes remain unmodified while Rust borrows them


-----------------------------
Copying a Borrowed C String
-----------------------------

**Convert only after the caller has established the contract**

.. code:: rust

  use std::ffi::{c_char, CStr};
  use std::str::Utf8Error;

  /// # Safety
  /// `ptr` is null or meets the C-string pointer contract
  unsafe fn copy_callsign(
      ptr: *const c_char,
  ) -> Result<Option<String>, Utf8Error> {
      if ptr.is_null() { return Ok(None); }
      // SAFETY: The caller upholds the pointer contract
      let callsign = unsafe { CStr::from_ptr(ptr) };
      callsign.to_str().map(|text| Some(text.to_owned()))
  }

-----------------------------
C String Conversion Results
-----------------------------

**C string conversion distinguishes absence, invalid encoding, and valid text**

* Null pointer returns :rust:`Ok(None)`
* Invalid UTF-8 returns :rust:`Err(Utf8Error)`
* Valid UTF-8 produces an owned :rust:`String`


------------------------
Passing a CString to C
------------------------

:rust:`CString` **owns NUL-terminated bytes with no interior NULs**

.. code:: rust

  use std::ffi::{c_char, CString};

  unsafe extern "C" {
      fn log_pilot(name: *const c_char);
  }

  fn main() {
      let pilot = CString::new("Maverick")
          .expect("literal contains no NUL");

      // SAFETY: `pilot` lives through the call
      // and C reads without retaining the pointer
      unsafe { log_pilot(pilot.as_ptr()) };
  }


--------------------------
CString Pointer Validity
--------------------------

**The pointer returned by** :rust:`CString::as_ptr()` **is valid only while its owner remains alive**

* :rust:`as_ptr()` does not transfer ownership
* The :rust:`CString` must remain alive until the C call returns
* C must not retain or modify the pointer
