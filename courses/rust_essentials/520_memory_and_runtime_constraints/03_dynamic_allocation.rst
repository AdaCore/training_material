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

--------------------------
  Length Is Not Capacity
--------------------------

.. code:: rust

  fn main() {
      let samples = Vec::<u16>::with_capacity(4);
      println!("length: {}", samples.len());
      println!("capacity: {}", samples.capacity());
 }

* **Length:** initialized elements currently stored

* **Capacity:** elements the buffer can hold before growing

.. warning::

  :rust:`with_capacity(4)` does not impose a maximum length

---------------------------------
  Clearing and Reusing Capacity
---------------------------------

.. code:: rust

  fn main() {
    let mut buf = Vec::<u16>::with_capacity(4);
    buf.push(42);
    println!("before: {:?}", (buf.len(), buf.capacity()));

    buf.clear();
    println!("after: {:?}", (buf.len(), buf.capacity()));
  }

* Each printed pair shows :rust:`(length, capacity)`

* :rust:`clear()` removes elements but retains the buffer for reuse

-------------------------
Observe Capacity Growth
-------------------------

.. code:: rust

  fn main() {
      let mut samples = Vec::<u16>::new();

      for value in 0..16 {
          let before = samples.capacity();
          samples.push(value);
          let after = samples.capacity();

          if after != before {
              println!("capacity: {before} -> {after}");
          }
      }
  }

* :rust:`push` may increase capacity when the buffer is full

* The growth strategy is not guaranteed

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

* Use :rust:`alloc` for optional :rust:`no_std` support

.. code:: rust

  #[cfg(feature = "alloc")]
  use alloc::{collections::TryReserveError, vec::Vec};

* Inside :rust:`try_sorted_samples`, reserve before copying

.. code:: rust

  let mut sorted = Vec::new();
  sorted.try_reserve(samples.len())?;
  for &sample in samples {
      sorted.push(sample);
  }

* :rust:`?` propagates a reported allocation failure

* Reserved slots suffice for every :rust:`u16` pushed here

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

* :rust:`try_reserve` can report capacity overflow or allocation failure

  * Other allocating operations can still follow a non-returning failure path

* Allocation failure behavior depends on allocator and runtime

* Allocation recovery requires a policy beyond adding :rust:`?`

.. warning::

  An allocator may abort rather than report failure. A fallible collection API cannot
  override that behavior
