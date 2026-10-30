==================================
Callbacks, Ownership, and Errors
==================================


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


--------------------------
Registering a C Callback
--------------------------

**The C API accepts a function pointer and an optional context**

.. code:: c

  // Uses the `log_fn` typedef from the earlier slide
  void register_logger(log_fn callback, void *context);

.. code:: rust

  unsafe extern "C" {
      fn register_logger(
          callback: Option<LogFn>,
          context: *mut c_void,
      );
  }

* Example contract: a null context is accepted
* C supplies a valid NUL-terminated message until each callback returns


------------------------------
Implementing a Rust Callback
------------------------------

**Use the callback's documented pointer contract**

.. code:: rust

  use std::ffi::CStr;

  unsafe extern "C" fn on_log(
      level: c_int,
      message: *const c_char,
      _context: *mut c_void,
  ) {
      // SAFETY: C provides a valid C string for this call
      let text = unsafe { CStr::from_ptr(message) };
      println!("{level}: {}", text.to_string_lossy());
  }


---------------------------
Passing the Callback to C
---------------------------

**The function pointer remains valid after registration**

.. code:: rust

  // SAFETY: C accepts a null context, and `on_log`
  // has a compatible ABI and program-long lifetime
  unsafe {
      register_logger(Some(on_log), core::ptr::null_mut());
  }

* The callback does not retain the borrowed message pointer
* Printing is illustrative; production callbacks need a failure and unwind policy


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
* **Reentrant calls**
  * A callback may be invoked again before a previous call returns

.. note::

  The function-pointer type encodes none of these execution guarantees
