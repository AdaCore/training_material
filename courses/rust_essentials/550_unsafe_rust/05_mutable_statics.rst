=================
Mutable Statics
=================

-------------------------------
Immutable and Mutable Statics
-------------------------------

**Static items provide data for the duration of the program**

* Reading an immutable static is safe
* :rust:`static mut` permits mutation through a global name
* Rust cannot track every access to that global state
* Each read or write requires an unsafe context

.. code:: rust

  static MESSAGE: &str = "Hello, world!";
  static mut COUNTER: u32 = 0;

.. warning::

  Prefer ownership or controlled access over global mutable state

---------------------------
Updating a Mutable Static
---------------------------

**The function's safety contract requires exclusive access**

.. code:: rust

  static mut RAPTORS_LOOSE: u32 = 0;

  /// # Safety
  /// Access to `RAPTORS_LOOSE` must be exclusive
  unsafe fn release_raptor() {
      // SAFETY: Required by this function's contract
      unsafe {
          RAPTORS_LOOSE += 1;
      }
  }

--------------------------------
Calling the Mutable-Static API
--------------------------------

**The caller must uphold the safety contract**

.. code:: rust

  fn main() {
      // SAFETY: Single-threaded exclusive access
      unsafe {
          release_raptor();
          let raptors = RAPTORS_LOOSE;
          println!("Raptors loose: {raptors}");
      }
  }

.. note::

  Rust 2024 rejects mutable-static references by default
