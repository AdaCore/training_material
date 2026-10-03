==============================
C Foreign Function Interface
==============================


-----------------
What Is an ABI?
-----------------

**API and ABI describe contracts at different levels**

* API - Application Programming Interface
  * Source-level contract
* ABI - Application Binary Interface
  * Compiled-code contract
  * Calling convention
  * Register and stack use
  * Argument and return representation
  * Data alignment
  * Symbol naming
  * Unwinding behavior


------------
extern "C"
------------

:rust:`extern "C"` **selects the platform's C ABI**

.. code:: rust

  unsafe extern "C" {
      fn foreign_function();
  }

* Implementation need not be written in C
* Declaration may still be wrong
* Pointer validity is not checked
* Memory safety is not guaranteed
* A required dynamic library may still be missing at runtime

.. note::

  :rust:`"C"` describes the ABI, not the implementation language


---------------------------
C-Compatible Scalar Types
---------------------------

**Match the foreign declaration exactly**

* Fixed-width integers
  * :C:`int32_t` / :C:`uint32_t` |rightarrow| :rust:`i32` / :rust:`u32`
  * :C:`int64_t` / :C:`uint64_t` |rightarrow| :rust:`i64` / :rust:`u64`
* Platform C types
  * :C:`int` / :C:`unsigned int` |rightarrow| :rust:`c_int` / :rust:`c_uint`
  * :C:`char` |rightarrow| :rust:`c_char`
  * :C:`void *` / :C:`const void *` |rightarrow| :rust:`*mut c_void` / :rust:`*const c_void`

.. note::

  C :C:`int` is not guaranteed to match Rust :rust:`i32` on every platform


--------------------
A Small C Function
--------------------

**C header defines the source-level boundary contract**

.. code:: c

  // rebel_math.h
  #include <stdint.h>

  int64_t calculate_jump_cost(
      int32_t distance,
      int32_t risk
  );

.. code:: c

  // rebel_math.c
  #include "rebel_math.h"

  int64_t calculate_jump_cost(
      int32_t distance,
      int32_t risk
  ) {
      return (int64_t)distance + ((int64_t)risk * 10);
  }


------------------------
Declaring the Function
------------------------

**Declare foreign functions in an unsafe external block**

.. code:: rust

  unsafe extern "C" {
      fn calculate_jump_cost(
          distance: i32,
          risk: i32,
      ) -> i64;
  }

* Symbol name: :rust:`calculate_jump_cost`
* C calling convention
* Two 32-bit signed integer inputs
* One 64-bit signed integer result


----------------------
Calling the Function
----------------------

**This foreign function is unsafe to call**

.. code:: rust

  fn main() {
      // SAFETY: The declaration matches `rebel_math.h`
      // and these scalar arguments have no extra preconditions
      let cost = unsafe {
          calculate_jump_cost(120, 4)
      };

      println!("Jump cost: {cost}");
  }

.. code:: output

  Jump cost: 160

* Functions declared in an external block are unsafe by default
* A declaration may be marked :rust:`safe` when every valid call is safe
* An idiomatic :rust:`// SAFETY:` comment explains
  * Why the declaration matches
  * Why the arguments are valid
  * Which lifetime or ownership rules apply


----------------------------
Linking Is a Separate Step
----------------------------

**Linking is separate from declaring the function**

* Common approaches
  * Build bundled C source with the Rust package
  * Link a system-installed static library
  * Link a system-installed dynamic library
  * Let a larger build system link Rust and native objects together
* Typical failures
  * Symbol not found
  * Wrong library search path
  * Debug/release library mismatch
  * Architecture mismatch
  * Incompatible C runtime or compiler toolchain


------------------------------
Configuring the Native Build
------------------------------

**Declare native build dependencies in** :filename:`Cargo.toml`

.. code:: toml

  [package]
  name = "rebel_math"
  version = "0.1.0"
  edition = "2024"

  [build-dependencies]
  cc = "1"

* :rust:`cc` is a build dependency
  * Invokes the available C compiler
* :filename:`build.rs` is the package build script
  * Can use :rust:`cc` to compile bundled C source

.. note::

  A working C compiler is an additional build prerequisite


------------------------
Compiling the C Source
------------------------

**Cargo runs** :filename:`build.rs` **before compiling the Rust target**

.. code:: rust

  fn main() {
      println!(
          "cargo::rerun-if-changed=native/rebel_math.c"
      );
      println!(
          "cargo::rerun-if-changed=native/rebel_math.h"
      );

      cc::Build::new()
          .file("native/rebel_math.c")
          .include("native")
          .compile("rebel_math");
  }

* :command:`cargo::rerun-if-changed` controls build-script reruns

.. note::

  :rust:`cc` builds a static library and emits Cargo metadata to link it


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

.. code:: rust

  mod raw {
      unsafe extern "C" {
          pub fn calculate_jump_cost(
              distance: i32,
              risk: i32,
          ) -> i64;
      }
  }

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
* Wrapper avoids mismatched raw arguments
* For an empty slice, C must not dereference :C:`values`


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


-------------------------------
Receiving a Borrowed C String
-------------------------------

**Borrowed C strings still need an explicit pointer contract**

* Non-null pointer names a readable NUL-terminated range
* Range stays within one allocation and below :rust:`isize::MAX` bytes
* Range remains unmodified during the call

.. code:: rust

  use std::ffi::{c_char, CStr};
  use std::str::Utf8Error;

  unsafe fn copy_callsign(
      ptr: *const c_char,
  ) -> Result<Option<String>, Utf8Error> {
      if ptr.is_null() {
          return Ok(None);
      }

      // SAFETY: Required by the caller contract
      let callsign = unsafe { CStr::from_ptr(ptr) };
      callsign
          .to_str()
          .map(|text| Some(text.to_owned()))
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

**Verify Rust and C layout agreement on every supported platform**

* :rust:`size_of::<DroidStatus>()` and :rust:`align_of::<DroidStatus>()`
* :rust:`std::mem::offset_of!(DroidStatus, field)` for field offsets
* C :C:`_Static_assert` checks
* Generated layout tests in the build or test pipeline


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


---------------------------
Convert in the Safe Layer
---------------------------

**Translate every raw value into a valid Rust value**

.. code:: rust

  #[derive(Debug, PartialEq, Eq)]
  pub enum DroidMode {
      Idle,
      Active,
      Damaged,
      Unknown(u32),
  }

  pub fn droid_mode(raw: u32) -> DroidMode {
      match raw {
          DROID_IDLE => DroidMode::Idle,
          DROID_ACTIVE => DroidMode::Active,
          DROID_DAMAGED => DroidMode::Damaged,
          other => DroidMode::Unknown(other),
      }
  }

.. note::

  :rust:`Unknown` preserves raw values that the safe layer does not yet recognize


-----------------
C Callback Type
-----------------

**C callbacks commonly use a function pointer plus a context pointer**

.. code:: c

  typedef void (*log_fn)(
      int level,
      const char *message,
      void *context
  );

* Context pointer carries state associated with the callback


--------------------
Rust Callback Type
--------------------

**Mirror the callback ABI and parameter types exactly**

.. code:: rust

  use std::ffi::{c_char, c_int, c_void};

  type LogFn = unsafe extern "C" fn(
      level: c_int,
      message: *const c_char,
      context: *mut c_void,
  );

* :rust:`unsafe` means callers must uphold the callback's pointer contracts
* A nullable callback can be represented as :rust:`Option<LogFn>`
* For FFI-compatible function pointers, :rust:`None` represents a null pointer


--------------------
Callback Contracts
--------------------

**Callback types do not encode the full callback contract**

* **Pointer validity**
  * Define nullability and access rules for :rust:`message` and :rust:`context`
* **Validity duration**
  * Define how long :rust:`message` and :rust:`context` remain valid
* **State and ownership**
  * Define who owns state behind :rust:`context`
* **Release point**
  * Define when callback state may be destroyed
* **Execution model**
  * Define calling threads, concurrency, and reentrancy


---------------------------------
Ownership Must Cross Explicitly
---------------------------------

**Every pointer contract must define ownership and when it changes**

* **Borrowed**
  * Valid for the call or until explicit unregister
* **Transferred to the callee**
  * Callee owns and destroys it
* **Returned to the caller**
  * Caller owns it and uses the matching destructor
* **Shared**
  * Define validity duration, ownership, and synchronization rules
* **Borrow end**
  * Define when the borrow ends

.. warning::

  "The other side probably frees it" is not an ownership model


----------------
Opaque Handles
----------------

**Opaque handles hide foreign implementation details**

.. code:: c

  // C
  typedef struct Holocron *HolocronHandle;

  HolocronHandle holocron_open(void);
  void holocron_close(HolocronHandle handle);

.. code:: rust

  // Rust raw layer
  mod raw {
      use std::ffi::c_void;

      unsafe extern "C" {
          pub fn holocron_open() -> *mut c_void;
          pub fn holocron_close(handle: *mut c_void);
      }
  }

* C callers see a distinct handle type without the struct definition
* Rust represents the opaque pointee as :rust:`c_void` in this raw layer
* Open and close functions define the resource lifetime explicitly


--------------------
Acquiring a Handle
--------------------

**Map a nullable foreign handle into a Rust invariant**

.. code:: rust

  use std::ffi::c_void;
  use std::ptr::NonNull;

  pub struct Holocron(NonNull<c_void>);

  impl Holocron {
      pub fn open() -> Option<Self> {
          // SAFETY: Declaration matches the header
          let ptr = unsafe { raw::holocron_open() };
          NonNull::new(ptr).map(Self)
      }
  }

* :rust:`NonNull::new` converts a possible null pointer into :rust:`Option`
* Every constructed :rust:`Holocron` contains a non-null foreign handle


--------------------
Releasing a Handle
--------------------

:rust:`Drop` **releases the foreign handle automatically**

.. code:: rust

  impl Drop for Holocron {
      fn drop(&mut self) {
          // SAFETY: Handle came from `holocron_open`
          // and is still owned by this wrapper
          unsafe { raw::holocron_close(self.0.as_ptr()) }
      }
  }

* Wrapper owns each successfully acquired handle
* Moving :rust:`Holocron` transfers ownership without duplicating the handle


----------------
C Status Codes
----------------

**C APIs often return status and write results through output pointers**

.. code:: c

  #include <stdint.h>

  int holocron_distance(
      HolocronHandle a,
      HolocronHandle b,
      uint32_t *out_distance
  );

* :C:`0` means success
* :C:`1` means no route is available
* :C:`2` means the result overflowed
* Other status values may appear if the library evolves


--------------------------------
Mirroring Status Codes in Rust
--------------------------------

**Raw layer preserves the foreign status-code contract**

.. code:: rust

  mod raw {
      use std::ffi::{c_int, c_void};

      unsafe extern "C" {
          pub fn holocron_distance(
              a: *mut c_void,
              b: *mut c_void,
              out_distance: *mut u32,
          ) -> c_int;
      }
  }

* Raw layer preserves the status code and output pointer
* Safe layer interprets them after the call


-------------------
Calling the C API
-------------------

**The safe wrapper provides Rust-owned output storage**

.. code:: rust

  let mut distance = 0;

  // SAFETY:
  // - `a` and `b` contain live `Holocron` handles
  // - `distance` is writable for one `u32`
  // - C does not retain the handles or output pointer
  let status = unsafe {
      raw::holocron_distance(
          a.0.as_ptr(),
          b.0.as_ptr(),
          &mut distance,
      )
  };

* :rust:`a` and :rust:`b` are borrowed :rust:`Holocron` values
* Rust retains ownership of the output storage
* :rust:`distance` is used only when the status reports success


------------------------------
Mapping the Status to Result
------------------------------

**Map known status codes and preserve unknown values**

.. code:: rust

  use std::ffi::c_int;

  #[derive(Debug, PartialEq, Eq)]
  pub enum HolocronError {
      NoRoute,
      Overflow,
      Unknown(c_int),
  }

  match status {
      0 => Ok(distance),
      1 => Err(HolocronError::NoRoute),
      2 => Err(HolocronError::Overflow),
      other => Err(HolocronError::Unknown(other)),
  }

* Public API exposes :rust:`Result`, not foreign status-code conventions
* Wrapper uses the output value only on successful status


---------------------------------
Do Not Unwind Across extern "C"
---------------------------------

**A normal** :rust:`extern "C"` **boundary is non-unwinding**

* **Rust panic**
  * Attempting to unwind through the boundary aborts the process
* **Foreign exception**
  * Unwinding into Rust through this ABI is undefined behavior
* **Expected failure**
  * Cross the boundary as explicit data
* **Containment rule**
  * Contain failures on their originating side

.. warning::

  Panic and exception policy is part of the boundary contract


------------------------
Threads and Reentrancy
------------------------

**Callback execution may not be single-threaded or one-at-a-time**

* **From another thread**
  * Ensure callback state is valid to access from that thread
* **Concurrently**
  * Synchronize shared callback state
* **Reentrantly**
  * Do not assume only one callback is active

.. note::

  The function-pointer type encodes none of these execution guarantees
