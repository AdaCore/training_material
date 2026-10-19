=================
Mutable Statics
=================

-------------------------------
Immutable and Mutable Statics
-------------------------------

**Static items provide data for the duration of the program**

* :rust:`static mut` declares a global variable that can be modified
  * Access to `static mut` requires safety guarantees from the programmer
  * Each read or write requires an unsafe context
* *Reading* an immutable static is safe

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


.. note::

  .. raw:: latex

    {\footnotesize

  Creating :rust:`static mut` references is rejected by default, even in :rust:`unsafe` blocks

  .. raw:: latex

    }
