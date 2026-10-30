=====================
Safe FFI Boundaries
=====================


--------------------------
Raw Layer and Safe Layer
--------------------------

**Keep raw declarations separate from the safe Rust interface**

.. image:: rust_essentials/600_raw_safe_layers.svg
   :width: 100%
   :align: center


----------------
A Safe Wrapper
----------------

**A safe wrapper hides the unsafe call from application code**

* **Raw Layer** - declares the foreign function

.. code:: rust

  mod raw {
      unsafe extern "C" {
          pub fn calculate_jump_cost(
              distance: i32,
              risk: i32,
          ) -> i64;
      }
  }

* **Safe Layer** - provides the safe Rust interface

.. code:: rust

  pub fn jump_cost(distance: i32, risk: i32) -> i64 {
      // SAFETY: Declaration matches the bundled C header
      unsafe { raw::calculate_jump_cost(distance, risk) }
  }

* Callers use :rust:`jump_cost(120, 4)` without an unsafe block
* Wrapper must establish every precondition required by the foreign call


-------------------
Pointer Contracts
-------------------

* A pointer parameter needs a complete safety contract
  * May it be null?
  * Is it readable, writable, or both?
  * How many elements are valid?
  * What alignment is required?
  * How long does the memory remain valid?
  * May the foreign function retain the pointer?
  * May another thread access the same memory?
  * Who owns and frees the memory?
* :rust:`*const T` and :rust:`*mut T` encode none of these guarantees


---------------------
Pointer Plus Length
---------------------

**C frequently represents a sequence as pointer plus length**

.. code:: c

  // C declaration
  #include <stddef.h>
  #include <stdint.h>

  int32_t crew_total(const int32_t *values, size_t length);

.. code:: rust

  // Rust declaration
  unsafe extern "C" {
      fn crew_total(
          values: *const i32,
          length: usize,
      ) -> i32;
  }

* Raw boundary preserves pointer and length as separate values
* Validity still depends on the pair being interpreted together

.. note::

  This example maps C :C:`size_t` to Rust :rust:`usize` for the supported target ABI


------------------------------
Wrapping Pointer Plus Length
------------------------------

**A safe wrapper can derive both raw arguments from a Rust slice**

.. code:: rust

  pub fn total(values: &[i32]) -> i32 {
      // SAFETY:
      // - `values` is readable for `values.len()` elements
      // - C reads only during the call
      // - C does not retain the pointer
      unsafe { crew_total(values.as_ptr(), values.len()) }
  }

* Slice keeps pointer and length consistent
* For an empty slice, C must not dereference :C:`values`
* C must provide defined behavior for all inputs, including signed overflow


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

.. warning::

  Do not pass :rust:`String` or :rust:`&str` directly through a C ABI


-----------------
FFI Without std
-----------------

**Calling foreign functions does not require** :rust:`std`

* :rust:`core::ffi` supplies C types and borrowed :rust:`CStr`
* :rust:`core::slice` supports constructing slices from raw parts
* :rust:`alloc` supplies owned :rust:`CString` and :rust:`String` when an allocator is available
* The following hosted examples use :rust:`std` imports for convenience


-------------------------------
Receiving a Borrowed C String
-------------------------------

**The caller of** :rust:`copy_callsign` **must establish the pointer contract**

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


----------------------------
Borrowed C String Outcomes
----------------------------

**Wrapper distinguishes null, invalid UTF-8, and valid text**

* :rust:`CStr::from_ptr` interprets the NUL-terminated byte sequence
* Null pointer maps to :rust:`Ok(None)`
* Invalid UTF-8 maps to :rust:`Err(Utf8Error)`
* Valid UTF-8 is copied into an owned :rust:`String`


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

.. warning::

  :rust:`CString::new` rejects interior NUL bytes


----------------------------
CString Borrowing Contract
----------------------------

**Borrowed pointer remains valid while the** :rust:`CString` **is alive**

* Ownership
  * :rust:`as_ptr()` borrows from :rust:`pilot`
* Validity
  * Pointer remains valid and read-only while :rust:`pilot` is alive
* Retention
  * Foreign code must not retain the pointer


-------------------------------
Struct Layout With #[repr(C)]
-------------------------------

**Use** :rust:`#[repr(C)]` **when a struct's layout is part of the C ABI**

.. code:: rust

  #[repr(C)]
  #[derive(Debug, Clone, Copy)]
  pub struct DroidStatus {
      pub model_id: u32,
      pub battery_percent: u8,
      pub active: u8,
      pub reserved: [u8; 2],
  }

* Default struct layout is not a stable C ABI contract
* :rust:`repr` means representation
  * :rust:`#[repr(C)]` defines C-compatible field ordering and layout rules
* Each field must also have an FFI-compatible representation
* :rust:`#[repr(C)]` does not validate field values or ownership


------------------------
Match the C Definition
------------------------

**C and Rust declarations must agree on field order and field types**

.. code:: c

  // C
  #include <stdint.h>

  typedef struct {
      uint32_t model_id;
      uint8_t battery_percent;
      uint8_t active;
      uint8_t reserved[2];
  } DroidStatus;

.. code:: rust

  // Rust
  #[repr(C)]
  pub struct DroidStatus {
      pub model_id: u32,
      pub battery_percent: u8,
      pub active: u8,
      pub reserved: [u8; 2],
  }


-----------------------------
Checking Layout Assumptions
-----------------------------

**Verify C and Rust layout compatibility at build or test time**

* Compare size, alignment, and field offsets
  * Rust: :rust:`size_of`, :rust:`align_of`, :rust:`offset_of!`
  * C: :C:`sizeof`, :C:`_Alignof`, :C:`offsetof`
* Check against the same expected layout
  * Compile-time assertions or automated layout tests
* Run checks in CI for every supported target and toolchain configuration


----------------------------------
Types to Keep Behind the Wrapper
----------------------------------

**Keep Rust-specific representations behind the wrapper**

* **Rust-owned containers**
  * :rust:`String` and :rust:`Vec<T>`
* **Borrowed Rust views**
  * Slices and references
* **Dynamic behavior**
  * Trait objects and closures
* **Rust-specific composition**
  * Tuples, enum variants with fields, and generics
* **Common boundary representations**
  * Scalars and integer status codes
  * Raw pointers and pointer-plus-length pairs
  * :rust:`#[repr(C)]` structs
  * Opaque handles


---------------------------------------
Foreign Enum-Like Values and Unknowns
---------------------------------------

**Use the integer type defined by the C API**

.. code:: c

  #include <stdint.h>

  typedef uint32_t DroidModeRaw;

  #define DROID_IDLE    ((DroidModeRaw)0)
  #define DROID_ACTIVE  ((DroidModeRaw)1)
  #define DROID_DAMAGED ((DroidModeRaw)2)

.. code:: rust

  pub const DROID_IDLE: u32 = 0;
  pub const DROID_ACTIVE: u32 = 1;
  pub const DROID_DAMAGED: u32 = 2;

* C API explicitly defines these values as 32-bit unsigned integers
* Unknown integers remain valid raw values


----------------------------
Representing Unknown Modes
----------------------------

**A Rust enum can preserve unrecognized foreign values**

.. code:: rust

  #[derive(Debug, PartialEq, Eq)]
  pub enum DroidMode {
      Idle,
      Active,
      Damaged,
      Unknown(u32),
  }

* :rust:`Unknown(u32)` retains future values without interpreting them as known variants


---------------------------
Convert in the Safe Layer
---------------------------

**Map known integers without discarding unknown values**

.. code:: rust

  pub fn droid_mode(raw: u32) -> DroidMode {
      match raw {
          DROID_IDLE => DroidMode::Idle,
          DROID_ACTIVE => DroidMode::Active,
          DROID_DAMAGED => DroidMode::Damaged,
          other => DroidMode::Unknown(other),
      }
  }

