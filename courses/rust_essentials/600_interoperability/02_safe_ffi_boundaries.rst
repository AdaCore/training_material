=====================
Safe FFI Boundaries
=====================


--------------------------------------
Separating FFI from Application Code
--------------------------------------

**A dedicated boundary isolates native declarations and unsafe operations from application code**

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

**A pointer contract defines the conditions for safely accessing memory through a pointer**

* **Validity** - Nullability, alignment, and the number of valid elements
* **Access** - Read/write permissions and initialization
* **Lifetime and ownership** - How long memory remains valid and who may release it
* **Concurrency** - Whether simultaneous accesses are permitted and how they are synchronized

* Raw pointer types :rust:`*const T` and :rust:`*mut T` do not enforce these conditions


------------------------------
Passing Contiguous Sequences
------------------------------

**C APIs commonly represent contiguous sequences with a pointer and a length**

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


---------------------------
Foreign Sequence Contract
---------------------------

**A safe Rust wrapper requires a documented C contract**

* :C:`crew_total` reads exactly :C:`length` initialized elements
* It does not modify or retain :C:`values`
* It does not dereference :C:`values` when :C:`length == 0`
* It defines the result for all inputs, including overflowing sums
  * The implementation must avoid signed-overflow undefined behavior
* These guarantees must be verified against the C implementation


-----------------------------------------
Wrapping a C Function with a Rust Slice
-----------------------------------------

**A safe wrapper derives both raw arguments from a Rust slice**

.. code:: rust

  pub fn total(values: &[i32]) -> i32 {
      // SAFETY: Slice provides a valid pointer and length
      // C reads the range without retaining the pointer
      // and handles empty inputs and overflow without UB
      unsafe { crew_total(values.as_ptr(), values.len()) }
  }

* The slice establishes the pointer/length relationship
* The wrapper is safe only if the C implementation meets its contract


----------------------------
C-Compatible Struct Layout
----------------------------

:rust:`repr` **means representation; use** :rust:`#[repr(C)]` **when a struct's layout is part of the C ABI**

.. code:: rust

  #[repr(C)]
  #[derive(Debug, Clone, Copy)]
  pub struct DroidStatus {
      pub model_id: u32,
      pub battery_percent: u8,
      pub active: u8,
      pub reserved: [u8; 2],
  }

* :rust:`#[repr(C)]` defines C-compatible field ordering and layout rules
* Ensure field types are FFI-compatible
* Verify layout on each target

.. warning::

  * Default struct layout is not a stable C ABI contract
  * :rust:`#[repr(C)]` does not validate field values or ownership


------------------------
Match the C Definition
------------------------

**C and Rust declarations must agree on field order and field types**

**C definition**

.. code:: c

  #include <stdint.h>

  typedef struct {
      uint32_t model_id;
      uint8_t battery_percent;
      uint8_t active;
      uint8_t reserved[2];
  } DroidStatus;

**Rust definition**

.. code:: rust

  #[repr(C)]
  pub struct DroidStatus {
      pub model_id: u32,
      pub battery_percent: u8,
      pub active: u8,
      pub reserved: [u8; 2],
  }


-----------------------------
Types at the C ABI Boundary
-----------------------------

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


-----------------------------
Handling C Enum-Like Values
-----------------------------

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


--------------------------
Choose a Narrow Boundary
--------------------------

* Good boundaries are narrow and explicit

  * Coarse-grained rather than one call per field access
  * Based on simple C-compatible data
  * Explicit about ownership
  * Small enough to review and test
  * Stable enough to version
  * Wrapped once rather than used directly throughout the crate

* Avoid mirroring an entire data model across the boundary

.. tip::

  Move complete operations across the boundary, not chatty calls
