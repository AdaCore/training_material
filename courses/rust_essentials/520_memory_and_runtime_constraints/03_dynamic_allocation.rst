====================
Dynamic Allocation
====================

-----------------------------
Where Allocation Can Happen
-----------------------------

.. list-table::
  :widths: 46 54
  :header-rows: 1

  * - Operation
    - Allocation behavior to consider
  * - :rust:`Vec::new()`, :rust:`String::new()`
    - Start empty without allocating a buffer
  * - Growing a :rust:`Vec` or :rust:`String`
    - May allocate or reallocate
  * - :rust:`Box::new(value)`
    - Allocates for a non-zero-sized value
  * - :rust:`format!`, :rust:`to_owned`, :rust:`clone`
    - May create new owned storage

* An allocation may be inside a helper, not at the call site

* Inspect the actual type and operation, not just its name

.. note::

  Cloning may allocate, depending on the type

------------------------
Length Is Not Capacity
------------------------

* Independent illustration inside a host :rust:`main`

.. code:: rust

  let samples = Vec::<u16>::with_capacity(4);
  assert_eq!(samples.len(), 0);
  assert!(samples.capacity() >= 4);

* **Length:** initialized elements currently stored

* **Capacity:** elements the buffer can hold before growing

.. warning::

  :rust:`with_capacity(4)` does not impose a maximum length

-------------------------------
Clearing and Reusing Capacity
-------------------------------

* Independent illustration inside a host :rust:`main`

.. code:: rust

  let mut samples = Vec::<u16>::with_capacity(4);
  samples.push(42);
  assert_eq!(samples.len(), 1);

  let capacity = samples.capacity();
  samples.clear();
  assert_eq!(samples.len(), 0);
  assert_eq!(samples.capacity(), capacity);

* :rust:`clear()` removes elements but retains the buffer for reuse

---------------------------
Growth Is a Runtime Event
---------------------------

* Growing a buffer may require obtaining and moving storage

* Allocator behavior can affect latency and memory overhead

* Fragmentation may matter, depending on the allocator and workload

* Reuse reserved storage when repeated work should not allocate

.. note::

  Amortized constant-time :rust:`push` is an average-cost bound over operations, not a
  maximum duration for every call

----------------------------------------
Fallible Allocation Uses Explicit APIs
----------------------------------------

.. list-table::
  :widths: 45 55
  :header-rows: 1

  * - API
    - What the caller receives
  * - :rust:`Vec::new()`
    - An empty :rust:`Vec`; no allocation to fail
  * - :rust:`Vec::with_capacity(n)`
    - A :rust:`Vec`, not a :rust:`Result`
  * - :rust:`values.try_reserve(n)`
    - :rust:`Result<(), TryReserveError>`

* :rust:`n` is additional capacity relative to the current length

* A failed reservation can report capacity overflow or allocator failure

.. tip::

  Fallible APIs report allocation failure as an error value

--------------------------------
Build an Owned Result Fallibly
--------------------------------

* Add to :filename:`src/memory.rs`

.. code:: rust

  #[cfg(feature = "alloc")]
  use alloc::{collections::TryReserveError, vec::Vec};

* Reserve before copying; propagate a reported failure with :rust:`?`

* The reserved slots suffice for every :rust:`u16` pushed here

-----------------------------------
Implementing "try_sorted_samples"
-----------------------------------

.. code:: rust

  #[cfg(feature = "alloc")]
  pub fn try_sorted_samples(
      samples: &[u16],
  ) -> Result<Vec<u16>, TryReserveError> {
      let mut sorted = Vec::new();
      sorted.try_reserve(samples.len())?;
      for &sample in samples {
          sorted.push(sample);
      }
      sorted.sort_unstable();
      Ok(sorted)
  }

-----------------------------------------
A "Result" Does Not Catch Every Failure
-----------------------------------------

* :rust:`try_reserve` handles failures the allocator reports to it

* Other allocating operations can still follow a non-returning failure path

* The final program determines that path's behavior

* Allocation recovery requires a policy beyond adding :rust:`?`

.. warning::

  An allocator may abort rather than report failure. A fallible collection API cannot
  override that behavior
