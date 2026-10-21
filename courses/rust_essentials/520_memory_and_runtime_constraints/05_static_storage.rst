================
Static Storage
================

----------------------------
Why Static Storage Matters
----------------------------

* Some data must remain available across many calls

* Immutable tables can be shared without recreating their contents

* Static storage makes that data independent of a call's local lifetime

* Add this read-only dataset to :filename:`src/memory.rs`

.. code:: rust

  pub static REFERENCE_SAMPLES: [u16; 4] = [10, 42, 7, 18];

.. note::

  Static storage lasts for the program's duration. Its placement in writable or
  read-only memory depends on the data and platform

----------------------------
Constants vs. Static Items
----------------------------

**A constant value and a static item are not the same thing**

* :rust:`const LIMIT: usize = 4;`

  * Defines a constant value, not a unique storage location

* :rust:`static TABLE: [u16; 4] = [0; 4];`

  * Defines an allocation with program-duration storage

* A local :rust:`[u16; LIMIT]` is still a local value

-----------------------
The 'static Lifetime
-----------------------

* :rust:`&'static [u16]` is a reference whose borrow can last for the program's duration

* A lifetime describes a borrow, not a physical storage region

* A :rust:`'static` annotation does not allocate memory

.. note::

  Fixed size, static storage, and :rust:`'static` are distinct concepts

-----------------------------------------
Persistent Does Not Mean Freely Mutable
-----------------------------------------

* Reading an ordinary immutable static is safe

* Reading or writing :rust:`static mut` requires additional safety obligations

* Shared mutation needs controlled access, not just persistent storage

* Prefer owned state passed with :rust:`&mut` when global sharing is unnecessary

.. warning::

  :rust:`static mut` is not a shortcut to a safe reusable buffer

-------------------------------------
Shared Static Data: A "std" Example
-------------------------------------

* Optional addition to :filename:`src/memory.rs`

.. code:: rust

  #[cfg(feature = "std")]
  pub mod shared {
      use std::sync::Mutex;

      static LAST_BATCH: Mutex<[u16; 4]> =
          Mutex::new([0; 4]);
  }

* The static binding stays immutable; mutation occurs through the lock

* Acquiring a lock can wait and affect runtime predictability

-----------------------------
Accessing the Shared Static
-----------------------------

* Add this function inside the :rust:`shared` module

.. code:: rust

  pub fn store(
      samples: [u16; 4],
  ) -> Result<(), &'static str> {
      let mut batch = LAST_BATCH.lock()
          .map_err(|_| "shared buffer poisoned")?;
      *batch = samples;
      Ok(())
  }
