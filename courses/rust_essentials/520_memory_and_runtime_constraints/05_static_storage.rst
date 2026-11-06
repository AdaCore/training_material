================
Static Storage
================

----------------------------
Why Static Storage Matters
----------------------------

* Some data must persist across many calls

  * Fixed lookup tables, shared counters, and status flags

* Static storage remains allocated for the program's duration

* Static items can hold read-only data or state with controlled updates

.. code:: rust

  // Read-only temperature thresholds
  pub static TEMPERATURE_THRESHOLDS: [i16; 3] = [0, 25, 80];

----------------------------
Constants vs. Static Items
----------------------------

* :rust:`const` names a value without guaranteeing one storage location

* :rust:`static` defines one location that exists for the program's duration

* Choose :rust:`static` when shared storage or a stable address is required

.. code:: rust

  const SAMPLE_COUNT: usize = 4;
  static SAMPLES: [u16; SAMPLE_COUNT] = [10, 42, 7, 18];

------------------------------
Returning 'static References
------------------------------

* :rust:`&'static T` can remain valid for the program's duration

* Useful when returning references to data with program-duration storage

.. code:: rust

  static TEMPERATURE_THRESHOLDS: [i16; 3] = [0, 25, 80];

  fn temperature_thresholds() -> &'static [i16] {
      &TEMPERATURE_THRESHOLDS
  }

* Returned reference remains valid after the function returns

* :rust:`'static` describes reference validity, not storage location

--------------------------
Controlling Static State
--------------------------

* Reading a read-only :rust:`static` item is safe

* Mutation of shared state requires controlled access

* Reading or writing :rust:`static mut` requires :rust:`unsafe`

  * Unsafe operations alone do not prevent data races

.. tip::

  Prefer passing owned state with :rust:`&mut` when global sharing is unnecessary
