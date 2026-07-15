========================================
Global Mutable State (Mutable Statics)
========================================

---------------------------------
Immutable and Mutable Statics
---------------------------------

Rust supports global variables called static variables

* Reading an immutable static is safe
* Reading or writing a :rust:`static mut` is an unsafe operation

  * Global mutable state can be accessed from multiple threads
  * Rust cannot automatically guarantee the required synchronization

.. code:: rust

  static MESSAGE: &str = "Hello, world!";
  static mut COUNTER: u32 = 0;

.. warning::

  Mutable statics are highly prone to data races and are generally best
  replaced with atomics, locks, or another safe synchronization primitive.

-----------------------------
Accessing a Mutable Static
-----------------------------

.. code:: rust

  static mut COUNTER: u32 = 0;

  /// Adds to the global counter.
  ///
  /// # Safety
  ///
  /// The caller must prevent concurrent and reentrant access to `COUNTER`.
  unsafe fn add_to_count(inc: u32) {
      unsafe {
          COUNTER += inc;
      }
  }

  /// Reads the global counter.
  ///
  /// # Safety
  ///
  /// The caller must prevent concurrent mutation of `COUNTER`.
  unsafe fn read_count() -> u32 {
      unsafe { COUNTER }
  }

  fn main() {
      // SAFETY: `main` performs all accesses sequentially on one thread.
      unsafe {
          add_to_count(3);
          println!("COUNTER: {}", read_count());
      }
  }

.. note::

  Rust 2024 rejects creating references to a mutable static by default.
  Direct reads and writes still require an unsafe block.
