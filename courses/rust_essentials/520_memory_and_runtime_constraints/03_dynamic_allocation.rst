====================
Dynamic Allocation
====================

-----------------------------
Where Allocation Can Happen
-----------------------------

.. list-table::
  :widths: 46 54
  :header-rows: 1

  * - **Operation**
    - **Allocation behavior to consider**
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

.. code:: rust

  fn main() {
      let samples = Vec::<u16>::with_capacity(4);
      println!("length: {}", samples.len());
      println!("capacity: {}", samples.capacity());
  }

* Example output (capacity may be larger on some implementations)

.. code:: text

  length: 0
  capacity: 4

* **Length:** initialized elements currently stored

* **Capacity:** elements the buffer can hold before growing

.. warning::

  :rust:`with_capacity(4)` does not impose a maximum length

-------------------------------
Clearing and Reusing Capacity
-------------------------------

.. code:: rust

  fn main() {
      let mut samples = Vec::<u16>::with_capacity(4);
      samples.push(42);
      println!("before: {:?}", (samples.len(), samples.capacity()));
      samples.clear();
      println!("after: {:?}", (samples.len(), samples.capacity()));
  }

* Example output (capacity may be larger on some implementations)

.. code:: text

  before: (1, 4)
  after: (0, 4)

* :rust:`clear()` removes elements but retains the buffer for reuse

-------------------------
Observe Capacity Growth
-------------------------

.. code:: rust

  fn main() {
      let mut samples = Vec::<u16>::new();
      for sample in 0..16 {
          let before = samples.capacity();
          samples.push(sample);
          if before != samples.capacity() {
              println!("capacity: {before} -> {}", samples.capacity());
          }
      }
  }

* Example output (growth strategy is not guaranteed)

.. code:: text

  capacity: 0 -> 4
  capacity: 4 -> 8
  capacity: 8 -> 16

* :rust:`push` may allocate when the buffer must grow

---------------------------
Growth Is a Runtime Event
---------------------------

* Growing a buffer may require obtaining and moving storage

* Allocator behavior can affect latency and memory overhead

* Heap fragmentation can make large contiguous allocations harder

* Reuse reserved storage to prevent unnecessary allocation

.. note::

  A :rust:`push` that fits within the current capacity is usually quick;
  one that must grow the buffer may take much longer

----------------------------------------
Fallible Allocation Uses Explicit APIs
----------------------------------------

.. list-table::
  :widths: 45 55
  :header-rows: 1

  * - API
    - Failure behavior
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

--------------------------------
Build an Owned Result Fallibly
--------------------------------

**The function reserves capacity before copying its input**

.. code:: rust

  let mut sorted = Vec::new();
  sorted.try_reserve(samples.len())?;
  for &sample in samples {
      sorted.push(sample);
  }

* A reported reservation failure occurs before any samples are copied

* The reserved capacity covers every :rust:`u16` pushed into the vector

* :rust:`?` propagates any error returned by :rust:`try_reserve`

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

-----------------------------------------
A "Result" Does Not Catch Every Failure
-----------------------------------------

* :rust:`try_reserve` can report capacity overflow or allocation failure

  * Other allocating operations can still follow a non-returning failure path

* Allocation failure behavior depends on allocator and runtime

* Allocation recovery requires a policy beyond adding :rust:`?`

.. warning::

  An allocator may abort rather than report failure. A fallible collection API cannot
  override that behavior
