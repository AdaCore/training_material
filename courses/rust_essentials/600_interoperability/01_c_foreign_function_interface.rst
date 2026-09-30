==============================
C Foreign Function Interface
==============================


-----------------
What Is an ABI?
-----------------

**ABI means Application Binary Interface**

* Calling convention
* Register and stack use
* Argument and return representation
* Data alignment
* Symbol naming
* Unwinding behavior
* An API is a source-level contract
* An ABI is a compiled-code contract


------------
extern "C"
------------

:rust:`extern "C"` **selects the platform's C calling convention**

.. code:: rust

  unsafe extern "C" {
      fn foreign_function(value: i32) -> i32;
  }

* The implementation need not be written in C
* The declaration may still be wrong
* Pointer validity is not checked
* Memory safety is not guaranteed
* The library may still be missing at runtime

.. note::

  :rust:`"C"` describes the ABI, not the implementation language


---------------------------
C-Compatible Scalar Types
---------------------------

**Match the foreign declaration exactly**

* **Fixed-width integers**
  * :C:`int32_t` / :C:`uint32_t` - :rust:`i32` / :rust:`u32`
  * :C:`int64_t` / :C:`uint64_t` - :rust:`i64` / :rust:`u64`
* **Platform C types**
  * :C:`int` / :C:`unsigned int` - :rust:`c_int` / :rust:`c_uint`
  * :C:`char` - :rust:`c_char`
  * :C:`void *` / :C:`const void *` - :rust:`*mut c_void` / :rust:`*const c_void`
* C :C:`int` is not guaranteed to be Rust :rust:`i32` on every platform


--------------------
A Small C Function
--------------------

**The C header defines the source-level boundary contract**

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


-------------------------------------
Declaring the Function in Rust 2024
-------------------------------------

**Rust 2024 requires** :rust:`unsafe extern` **blocks**

.. code:: rust

  unsafe extern "C" {
      fn calculate_jump_cost(
          distance: i32,
          risk: i32,
      ) -> i64;
  }

* The symbol name is :rust:`calculate_jump_cost`
* The calling convention is C
* The inputs are two 32-bit signed integers
* The result is one 64-bit signed integer


----------------------
Calling the Function
----------------------

**Calling the foreign function requires an unsafe context**

.. code:: rust

  fn main() {
      // SAFETY: The declaration matches `rebel_math.h`
      // and these scalar arguments have no extra preconditions
      let cost = unsafe {
          calculate_jump_cost(120, 4)
      };

      println!("Jump cost: {cost}");
  }

:command:`Jump cost: 160`

* A useful :rust:`// SAFETY:` comment explains
  * Why the declaration matches
  * Why the arguments are valid
  * Which lifetime or ownership rules apply


----------------------------
Linking Is a Separate Step
----------------------------

**Linking is separate from declaring the function**

* **Common approaches**
  * Build bundled C source with the Rust package
  * Link a system-installed static library
  * Link a system-installed dynamic library
  * Let a larger build system link Rust and native objects together
* **Typical failures**
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

* Add :rust:`cc` under :filename:`[build-dependencies]`
* :filename:`build.rs` can then use it to compile bundled C source

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

.. note::

  :rust:`cc` compiles the bundled C source as part of the build


--------------------------
Raw Layer and Safe Layer
--------------------------

**Keep raw declarations separate from the safe Rust interface**

.. container:: columns

  .. container:: column
      :width: 64%

    .. image:: comprehensive_rust_training/600_raw_safe_layers.svg
       :width: 100%

  .. container:: column
      :width: 36%

    * **Safe layer**
      * Enforces Rust invariants
    * **Raw layer**
      * Mirrors the foreign header


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
* The wrapper owns the responsibility for the foreign preconditions


-------------------
Pointer Contracts
-------------------

**A pointer parameter needs a complete safety contract**

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
  int32_t crew_total(const int32_t *values, size_t length);

.. code:: rust

  // Rust declaration
  unsafe extern "C" {
      fn crew_total(
          values: *const i32,
          length: usize,
      ) -> i32;
  }

* The raw boundary preserves pointer and length as separate values
* Validity still depends on the pair being interpreted together


------------------------------
Wrapping Pointer Plus Length
------------------------------

**A safe wrapper can derive both raw arguments from a Rust slice**

.. code:: rust

  pub fn total(values: &[i32]) -> i32 {
      // SAFETY: Both arguments come from
      // the same live slice
      // C only reads during the call
      unsafe { crew_total(values.as_ptr(), values.len()) }
  }

* The slice keeps pointer and length consistent
* The wrapper avoids mismatched raw arguments

.. note::

  Validate that C :C:`size_t` is ABI-compatible with Rust :rust:`usize`


--------------------------------
C Strings Are Not Rust Strings
--------------------------------

**C strings and Rust strings have different representations**

* **C string**

  * Pointer to bytes
  * Terminated by a zero byte (:dfn:`NUL`)
  * Not automatically UTF-8
  * Valid only while its backing storage exists

* **Rust owned string**

  * :rust:`String` is owned and growable
  * UTF-8
  * Uses Rust-specific internal fields

.. warning::

  Do not pass :rust:`String` or :rust:`&str` directly through a C ABI


-------------------------------
Receiving a Borrowed C String
-------------------------------

**Borrowed C strings still need an explicit pointer contract**

* Non-null pointers refer to readable bytes in one allocation
* A NUL terminator appears within :rust:`isize::MAX` bytes
* The bytes remain unmodified for the duration of the call

.. code:: rust

  use std::ffi::{c_char, CStr};

  unsafe fn copy_callsign(ptr: *const c_char) -> Option<String> {
      if ptr.is_null() { return None; }

      // SAFETY: Required by the caller contract
      let callsign = unsafe { CStr::from_ptr(ptr) };
      callsign.to_str().ok().map(str::to_owned)
  }

* :rust:`CStr::from_ptr` interprets the NUL-terminated byte sequence
* :rust:`to_str` checks that those bytes are valid UTF-8


------------------------
Passing a CString to C
------------------------

:rust:`CString` **owns NUL-terminated bytes with no interior NULs**

.. code:: rust

  use std::ffi::{c_char, CString};

  unsafe extern "C" {
      fn log_pilot(name: *const c_char);
  }

  let pilot = CString::new("Maverick")
      .expect("literal contains no NUL");

  // SAFETY: `pilot` lives through the call and C only reads
  unsafe { log_pilot(pilot.as_ptr()) }

* **Ownership** - :rust:`as_ptr()` borrows from :rust:`pilot`
* **Validity** - read-only while :rust:`pilot` remains alive
* **Retention** - foreign code must not retain the pointer

.. warning::

  :rust:`CString::new` rejects interior NUL bytes


-------------------------------
Struct Layout With #[repr(C)]
-------------------------------

**Use** :rust:`#[repr(C)]` **for structs that cross a C boundary**

.. code:: rust

  #[repr(C)]
  #[derive(Debug, Clone, Copy)]
  pub struct DroidStatus {
      pub model_id: u32,
      pub battery_percent: u8,
      pub active: u8,
      pub reserved: [u8; 2],
  }

* Rust's default struct layout is not a stable C ABI contract
* :rust:`#[repr(C)]` defines C-compatible field ordering and layout rules
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

**Verify layout on every supported platform**

* :rust:`size_of::<DroidStatus>()` and :rust:`align_of::<DroidStatus>()`
* C :C:`_Static_assert` checks
* Generated layout tests in the build or test pipeline


----------------------------------
Types to Keep Behind the Wrapper
----------------------------------

**Keep Rust-specific representations behind the wrapper**

* **Rust-managed storage** - :rust:`String`, :rust:`Vec<T>`, slices, and references
* **Dynamic behavior** - trait objects and closures
* **Rust-specific composition** - tuples, enum variants with fields, and generics
* **Common boundary representations**
  * Scalars and integer status codes
  * Raw pointers and pointer-plus-length pairs
  * :rust:`#[repr(C)]` structs
  * Opaque handles


----------------------------------
Foreign Enums and Unknown Values
----------------------------------

**Keep the raw boundary integer-based**

* C APIs may pass integer values added by a newer library
* A Rust enum cannot safely represent an unknown discriminant

.. code:: rust

  pub const DROID_IDLE: u32 = 0;
  pub const DROID_ACTIVE: u32 = 1;
  pub const DROID_DAMAGED: u32 = 2;

* Unknown integers remain valid raw values
* The safe layer can map them to :rust:`Unknown(...)`


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

  :rust:`Unknown` preserves forward compatibility


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

* The context pointer carries state associated with the callback
* The function pointer omits lifetime and threading rules


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

* A nullable callback can be represented as :rust:`Option<LogFn>`
* For FFI-compatible function pointers, :rust:`None` represents a null pointer


--------------------
Callback Contracts
--------------------

**Callback types do not encode the full callback contract**

* **Lifetime**
  * :rust:`message` / :rust:`context` validity and callback storage
* **State and ownership**
  * State behind :rust:`context` and its owner
* **Release point**
  * When callback state may be destroyed
* **Execution model**
  * Calling threads, overlap, and reentrancy


---------------------------------
Ownership Must Cross Explicitly
---------------------------------

**Every pointer contract must define ownership and when it changes**

* **Borrowed** - valid for the call or until explicit unregister
* **Transferred to the callee** - the callee owns and destroys it
* **Returned to the caller**
  * The caller owns it and uses the matching destructor
* **Shared** - define reference-count and synchronization rules
* **Borrow end** - every borrow needs an explicit end

.. warning::

  "The other side probably frees it" is not an ownership model


----------------
Opaque Handles
----------------

**Opaque handles hide foreign implementation details**

.. code:: c

  // C
  typedef void *HolocronHandle;

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

* Rust does not need to know the foreign instance's internal layout
* Open and close functions define the lifetime boundary explicitly


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

* The wrapper owns each successfully acquired handle
* Moving :rust:`Holocron` transfers ownership without duplicating the handle


----------------
C Status Codes
----------------

**C APIs often combine a status code with an output pointer**

.. code:: c

  int portal_distance(
      const Portal *a,
      const Portal *b,
      uint32_t *out_distance
  );

* :C:`0` means success
* :C:`1` means a null argument was rejected
* :C:`2` means the result overflowed
* Other values may appear if the library evolves
* The raw layer preserves both the integer status and the output value


-------------------
Calling the C API
-------------------

**The safe wrapper provides Rust-owned output storage**

.. code:: rust

  let mut distance = 0;

  // SAFETY: `Portal` keeps both handles valid
  // and `distance` is writable output storage
  let status = unsafe {
      raw::portal_distance(
          a.as_ptr(),
          b.as_ptr(),
          &mut distance,
      )
  };

* The call produces a status code and an output value
* Rust retains ownership of the output storage


------------------------------
Mapping the Status to Result
------------------------------

**Translate every foreign status code into a valid Rust result**

.. code:: rust

  #[derive(Debug, PartialEq, Eq)]
  pub enum PortalError {
      NullArgument,
      Overflow,
      Unknown(i32),
  }

  match status {
      0 => Ok(distance),
      1 => Err(PortalError::NullArgument),
      2 => Err(PortalError::Overflow),
      other => Err(PortalError::Unknown(other)),
  }

* The public API exposes :rust:`Result`, not foreign status-code conventions
* The wrapper uses the output value only on successful status


---------------------------------
Do Not Unwind Across extern "C"
---------------------------------

**A normal** :rust:`extern "C"` **boundary is non-unwinding**

* **Rust panic** - reaching the boundary aborts the process
* **Foreign exception**
  * Unwinding into Rust through this ABI is undefined behavior
* **Expected failure** - cross the boundary as explicit data
* **Containment rule** - contain failures on their originating side

.. warning::

  Panic and exception policy is part of the ABI contract


------------------------
Threads and Reentrancy
------------------------

**Callback execution may not be single-threaded or one-at-a-time**

* **From another thread** - enforce the library's thread-affinity rules
* **Concurrently** - synchronize shared callback state
* **Reentrantly** - do not assume only one callback is active

.. note::

  The function-pointer type encodes none of these execution guarantees
