=======================
Callbacks in Practice
=======================


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
