====================
Dynamic Allocation
====================

---------------------
Allocation Behavior
---------------------

.. list-table::
  :widths: 44 56
  :header-rows: 1

  * - **Operation**
    - **Heap Allocation**
  * - :rust:`Vec::new()`, :rust:`String::new()`
    - No allocation
  * - :rust:`Vec::push()`, :rust:`String::push_str()`
    - May allocate when capacity is insufficient
  * - :rust:`Box::new(value)`
    - Allocates for non-zero-sized values
  * - :rust:`format!`, :rust:`to_owned`, :rust:`clone`
    - May allocate, depending on the operation and type

.. warning::

  Heap allocations can occur inside helper functions

------------------------
Length Is Not Capacity
------------------------

* :rust:`Vec::with_capacity(4)` starts empty with capacity at least 4

.. code:: rust

  fn main() {
      let samples = Vec::<u16>::with_capacity(4);
      println!("length: {}", samples.len());
      println!("capacity: {}", samples.capacity());
  }

* Example output

.. code:: output

  length: 0
  capacity: 4

* **Length:** number of initialized elements currently stored

* **Capacity:** number of elements the buffer can hold before growing

.. note::

  Requested capacity is a minimum, not a maximum

-------------------------------
Clearing and Reusing Capacity
-------------------------------

.. code:: rust

  fn main() {
      let mut samples = Vec::<u16>::with_capacity(4);
      samples.push(42);
      let before = (samples.len(), samples.capacity());
      println!("before: {before:?}");
      samples.clear();
      let after = (samples.len(), samples.capacity());
      println!("after: {after:?}");
  }

* Example output

.. code:: output

  before: (1, 4)
  after: (0, 4)

* :rust:`clear()` removes elements but retains the buffer for reuse

---------------------
When Capacity Grows
---------------------

.. code:: rust

  fn main() {
      let mut samples = Vec::<u16>::new();
      for sample in 0..16 {
          let before = samples.capacity();
          samples.push(sample);
          let after = samples.capacity();
          if before != after {
              println!("sample {sample}: {before} -> {after}");
          }
      }
  }

* Example output (growth strategy may vary)

.. code:: output

  sample 0: 0 -> 4
  sample 4: 4 -> 8
  sample 8: 8 -> 16

* Capacity changes only on some pushes, not with every sample

--------------------------
Costs of Capacity Growth
--------------------------

* **Trigger:** insufficient capacity for a new element

* **Possible work:** grow heap storage, possibly moving elements

* **Consequences:**

  * Variable execution time and additional memory overhead

  * Allocation failure and heap fragmentation risks

* **Mitigation:** reserve sufficient capacity before time-critical operations

----------------------------------------
Fallible Allocation Uses Explicit APIs
----------------------------------------

.. list-table::
  :widths: 45 55
  :header-rows: 1

  * - **API**
    - **Failure Behavior**
  * - :rust:`Vec::new()`
    - No allocation
  * - :rust:`Vec::with_capacity(n)`
    - Panics on overflow; may abort on allocation failure
  * - :rust:`values.try_reserve(n)`
    - Returns :rust:`Err(TryReserveError)` on failure

* :rust:`with_capacity(n)` requests space for at least :rust:`n` elements

* :rust:`try_reserve(n)` ensures capacity for at least :rust:`len + n` elements

.. tip::

  :rust:`try_reserve` can report capacity overflow or allocator-reported failure

------------------------
Reserve Before Copying
------------------------

* :rust:`try_reserve` requests enough capacity for all input samples

.. code:: rust

  let mut sorted = Vec::new();
  sorted.try_reserve(samples.len())?;
  for &sample in samples {
      sorted.push(sample);
  }

* A reservation error occurs before any samples are copied

* A successful reservation avoids capacity growth during the loop

* :rust:`?` propagates a reservation error to the caller

-----------------------------------
Fallible Sorting Returns a Result
-----------------------------------

.. code:: rust

  use std::collections::TryReserveError;

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

-------------------------------
Limits of Fallible Allocation
-------------------------------

* :rust:`try_reserve` reports capacity overflow or allocator-reported failure

* Fallible allocation APIs cannot guarantee recovery

  * An allocator may abort instead of reporting failure

  * :rust:`Vec::with_capacity` may abort on allocation failure
