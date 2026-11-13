==================================
Callbacks, Ownership, and Errors
==================================


--------------------
Callback Contracts
--------------------

**A callback lets foreign code invoke a function supplied by Rust**

* **Function-pointer type**
  * Matches the C ABI but does not express every safety obligation
* **Pointer validity and duration**
  * Define nullability, access rights, and validity duration
* **State and ownership**
  * Define who owns state behind a context pointer
* **Release point**
  * Define when registration ends and state may be destroyed
* **Execution model**
  * Define whether callbacks run synchronously or may be invoked later


--------------------------
A Synchronous C Callback
--------------------------

**C receives and invokes a function pointer supplied by its caller**

.. code:: c

  #include <stdint.h>

  typedef int32_t (*transform_fn)(int32_t);

  int32_t apply_transform(
      int32_t value,
      transform_fn callback
  ) {
      return callback(value);
  }

* The caller supplies a valid callback with a matching signature
* C invokes the callback once before returning
* C does not retain the function pointer


----------------------------
Passing a Rust Callback to C
----------------------------

**A Rust callback uses the same C ABI and signature**

.. code:: rust

  unsafe extern "C" {
      fn apply_transform(
          value: i32,
          callback: extern "C" fn(i32) -> i32,
      ) -> i32;
  }

  extern "C" fn is_even(value: i32) -> i32 {
      if value % 2 == 0 { 1 } else { 0 }
  }

  pub fn check_even(value: i32) -> i32 {
      // SAFETY: The declaration matches the C implementation
      // and C calls `is_even` once without retaining it
      unsafe { apply_transform(value, is_even) }
  }

* The callback's ABI and parameter types match the C declaration
* The callback accepts every possible :rust:`i32` argument

-----------------------------------
Ownership Across the FFI Boundary
-----------------------------------

**Every pointer contract must define ownership and when it changes**

* **Borrowed**
  * Valid for the call or until a documented release point
* **Ownership transferred to the callee**
  * Callee owns and destroys it
* **Ownership transferred to the caller**
  * Caller must destroy the returned resource with the matching destructor
* **Shared**
  * Define validity duration, ownership, and synchronization rules

.. warning::

  Never assume which side is responsible for releasing a resource


----------------
Opaque Handles
----------------

**Opaque handles let code refer to resources without knowing their internal representation**

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

* Rust represents the opaque pointer as :rust:`*mut c_void` in the raw layer
* :rust:`*mut c_void` does not guarantee validity or establish ownership


-------------------------
Foreign Handle Contract
-------------------------

**For this example, the C API guarantees**

* :C:`holocron_open()` has no caller-side safety preconditions
  * Returns null on failure or a fresh, valid, uniquely owned handle
* Every successful handle must be closed exactly once
* :C:`holocron_close()` releases a live, owned handle
* No other code invalidates a handle while the wrapper owns it
* Other operations borrow handles without taking ownership


--------------------
Acquiring a Handle
--------------------

**Use a Rust wrapper to manage the handle returned by the C API**

.. code:: rust

  use std::ffi::c_void;
  use std::ptr::NonNull;

  pub struct Holocron(NonNull<c_void>);

  impl Holocron {
      pub fn open() -> Option<Self> {
          // SAFETY: Declaration matches the C header
          // and holocron_open has no caller-side preconditions
          let ptr = unsafe { raw::holocron_open() };
          NonNull::new(ptr).map(Self)
      }
  }

* :rust:`NonNull::new` returns :rust:`None` for null and :rust:`Some` otherwise

.. warning::

  :rust:`NonNull` does not guarantee handle validity, resource lifetime, or ownership


--------------------
Releasing a Handle
--------------------

:rust:`Drop` **releases the foreign handle automatically**

.. code:: rust

  impl Drop for Holocron {
      fn drop(&mut self) {
          // SAFETY: Handle came from holocron_open
          // and is still live and exclusively owned
          unsafe { raw::holocron_close(self.0.as_ptr()) }
      }
  }

* :rust:`Drop` calls :rust:`holocron_close` when the wrapper is dropped
* Moving :rust:`Holocron` transfers responsibility for closing the handle without closing it


----------------
C Status Codes
----------------

**Rust wrappers must handle unknown status codes from evolving C APIs**

.. code:: c

  #include <stdint.h>

  int holocron_distance(
      HolocronHandle a,
      HolocronHandle b,
      uint32_t *out_distance
  );

* :C:`0` means success

  * C writes a valid result through :C:`out_distance`

* :C:`1` means no route is available
* :C:`2` means the result overflowed


-----------------------------------
Matching the C Function Signature
-----------------------------------

**Raw Rust declaration mirrors the C function's ABI-level signature**

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
  // - Handles and the output pointer are not retained by the C API
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

**Safe wrappers translate C status codes into** :rust:`Result` **values**

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


----------------------
Unwinding Across FFI
----------------------

:rust:`extern "C"` **boundaries do not support unwinding**

* **Rust panic**
  * Attempting to unwind through the boundary aborts the process
* **Foreign exception**
  * Unwinding into Rust through this ABI is undefined behavior
* **Expected failure**
  * Cross the boundary as explicit data
* **Containment rule**
  * Keep Rust panics in Rust code and foreign exceptions in foreign code

.. warning::

  Panic and exception policy is part of the boundary contract
