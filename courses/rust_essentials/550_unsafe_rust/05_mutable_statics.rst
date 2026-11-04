=================
Mutable Statics
=================

-------------------------------
Immutable and Mutable Statics
-------------------------------

**Static items provide data for the duration of the program**

* :rust:`static` declares an *immutable* static item

  * Reading its value is safe

* :rust:`static mut` declares a *mutable* static item

  * Its value can be changed
  * Reading or writing requires an unsafe context
  * The programmer must prevent conflicting accesses

.. code:: rust

  static MESSAGE: &str = "Hello, world!";
  static mut COUNTER: u32 = 0;

---------------------------
Updating a Mutable Static
---------------------------

**Safety contract requires exclusive access**

.. code:: rust

  static mut RAPTORS_LOOSE: u32 = 0;

  /// # Safety
  /// Access to 'RAPTORS_LOOSE' must be exclusive
  unsafe fn release_raptor() {
      // SAFETY: Required by this function's contract
      unsafe {
          RAPTORS_LOOSE += 1;
      }
  }

--------------------------------
Calling the Mutable-Static API
--------------------------------

**Caller must uphold the safety contract**

.. code:: rust

  // SAFETY: Single-threaded exclusive access
  unsafe {
      release_raptor();
      let raptors = RAPTORS_LOOSE;
      println!("Raptors loose: {raptors}");
  }


.. warning::

  Avoid creating references to :rust:`static mut`, even inside :rust:`unsafe` blocks
