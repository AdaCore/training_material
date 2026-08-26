=================
Mutable Statics
=================

-------------------------------
Immutable and Mutable Statics
-------------------------------

**Static variables store data for the lifetime of the program**

* Reading an immutable static is safe
* :rust:`static mut` permits mutation through a global name
* Rust cannot track every access to that global state
* Each read or write requires an unsafe context

.. code:: rust

  static MESSAGE: &str = "Hello, world!";
  static mut COUNTER: u32 = 0;

.. warning::

  Global mutable state weakens local reasoning; prefer ownership or a small access-control abstraction.

----------------------------
Accessing a Mutable Static
----------------------------

.. code:: rust

  static mut COUNTER: u32 = 0;

  /// # Safety
  ///
  /// Access to `COUNTER` must not overlap or be reentrant.
  unsafe fn add_to_count(inc: u32) {
      // SAFETY: Required by this function's contract.
      unsafe { COUNTER += inc; }
  }

  fn main() {
      // SAFETY: Access is exclusive and non-reentrant.
      unsafe {
          add_to_count(3);
          println!("COUNTER: {}", COUNTER);
      }
  }

.. note::

  Rust 2024 rejects mutable-static references by default.
