=====================
Runtime Constraints
=====================

---------------------------------------
One Operation, Three Memory Contracts
---------------------------------------

**Choose the API contract before choosing the container**

.. image:: rust_essentials/520_sorting_api_contracts.svg
   :width: 100%
   :align: center

* All three preserve the input and sort the same :rust:`u16` values

* Ownership, output storage, and failure behavior differ

-----------------------------------
Bound Memory, Then Bound the Work
-----------------------------------

* Reject inputs that exceed the supported size limit

* Reuse storage when repeated allocation is not acceptable

* Budget for comparisons, copying, cleanup, and synchronization

* Use measurements and analysis for deployment environment

.. warning::

  No heap allocation does not mean constant-time execution. Sorting still depends on
  how many elements are processed

-----------------------------
Choose an Exhaustion Policy
-----------------------------

* **Reject:** return an error without returning a partial result

* **Degrade:** perform a smaller, explicitly supported operation

* **Drop data:** discard according to a documented application policy

* **Retry:** only when there is a reason the resource may become available

.. note::

  Replacing :rust:`Result` handling with unconditional :rust:`unwrap()` changes the
  policy to panicking on that error

----------------------------------------
Check Both Sides of the Capacity Limit
----------------------------------------

* Create :filename:`tests/memory.rs`

.. code:: rust

  use sample_tools::memory::{
      bounded_sorted, maximum_report,
      sort_into, BufferTooSmall,
  };

* Exact capacity succeeds; one extra element is rejected

* A rejected caller-buffer request leaves its existing contents unchanged

----------------------------------
Check the Caller-Provided Buffer
----------------------------------

* Add to :filename:`tests/memory.rs`

.. code:: rust

  #[test]
  fn checks_output_capacity() {
      let mut scratch = [0_u16; 2];
      assert_eq!(sort_into(&[42, 7], &mut scratch),
          Ok(&[7, 42][..]));
      assert_eq!(sort_into(&[1, 2, 3], &mut scratch),
          Err(BufferTooSmall));
      assert_eq!(scratch, [7, 42]);
  }

---------------------------------
Check the Fixed-Capacity Result
---------------------------------

* Add to :filename:`tests/memory.rs`

.. code:: rust

  #[test]
  fn checks_fixed_capacity() {
      let values = bounded_sorted::<2>(&[42, 7]).unwrap();
      assert_eq!(values.as_slice(), &[7, 42]);
      assert_eq!(bounded_sorted::<2>(&[1, 2, 3]),
          Err(BufferTooSmall));
  }

--------------------
Check Empty Inputs
--------------------

* Add to :filename:`tests/memory.rs`

.. code:: rust

  #[test]
  fn checks_empty_input() {
      assert!(bounded_sorted::<0>(&[]).unwrap().is_empty());
      assert!(bounded_sorted::<0>(&[1]).is_err());

      let mut scratch = [];
      assert_eq!(sort_into(&[], &mut scratch), Ok(&[][..]));
  }

* Zero capacity is still a meaningful boundary case

* Test failure without exhausting the machine's memory

--------------------------------
Check the Text Buffer Capacity
--------------------------------

* Add to :filename:`tests/memory.rs`

.. code:: rust

  #[test]
  fn checks_text_capacity() {
      let report = maximum_report::<16>(42).unwrap();
      assert_eq!(report.as_str(), "Maximum: 42");
      assert!(maximum_report::<4>(42).is_err());
  }

---------------------------------------
Check the Allocating API When Enabled
---------------------------------------

* Add to :filename:`tests/memory.rs`

.. code:: rust

  #[cfg(feature = "alloc")]
  #[test]
  fn checks_heap_backed_result() {
      use sample_tools::memory::try_sorted_samples;

      let sorted = try_sorted_samples(&[42, 7]).unwrap();
      assert_eq!(sorted.as_slice(), &[7, 42]);
      assert!(try_sorted_samples(&[]).unwrap().is_empty());
  }

* Exercises the :rust:`alloc`-gated API when that feature is enabled

* A successful call does not test allocator exhaustion

----------------------------------
Check the Library Configurations
----------------------------------

* Run from :rust:`sample_tools`, after applying the additions

.. code:: sh

  cargo check --lib --no-default-features
  cargo check --lib --no-default-features --features alloc
  cargo check --lib

* Bounded APIs work with all three configurations

* :rust:`try_sorted_samples` needs :rust:`alloc`; shared :rust:`Mutex` needs :rust:`std`

----------------------------------
Test Every Library Configuration
----------------------------------

* Run from :rust:`sample_tools`

.. code:: sh

  cargo test --test memory --no-default-features
  cargo test --test memory --no-default-features \
    --features alloc
  cargo test --test memory

* Capacity tests run in every configuration; allocating API tests need :rust:`alloc`

.. note::

  Compilation and host tests do not prove memory or timing bounds

-----------------------
What We Did Not Cover
-----------------------

* **Allocator Implementation**

  * Custom allocators, memory pools, and allocator internals

* **Whole-Program Analysis**

  * Exact stack usage and worst-case execution-time analysis

* **Runtime and Platform Setup**

  * Startup code, linker placement, and panic-handler implementation

* **Advanced Shared State**

  * Interrupt-safe storage, synchronization design, and DMA buffers

.. note::

  A bounded data structure is one part of a resource-aware design, not a complete
  system guarantee
