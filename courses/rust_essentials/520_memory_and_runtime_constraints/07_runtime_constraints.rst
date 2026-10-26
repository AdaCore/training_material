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

* Include copying, sorting, and cleanup in the execution budget

* Use measurements and analysis for deployment environment

.. warning::

  The cost of :rust:`sort_unstable` depends on the slice's length, not the
  collection's capacity; avoiding heap allocation does not make it constant-time

-----------------------------------
Handle Memory and Capacity Limits
-----------------------------------

* **Reject:** return an error without returning a partial result

  * :rust:`unwrap()` panics on :rust:`Err` instead of returning the error

* **Degrade:** perform a smaller, explicitly supported operation

* **Drop data:** discard according to a documented application policy

* **Retry:** only when there is a reason the resource may become available

-----------------------------------
Capacity Limits: Observed Results
-----------------------------------

* Host-side example using the default :rust:`std` feature

.. code:: rust

  let mut scratch = [0_u16; 2];
  println!("{:?}", sort_into(&[42, 7], &mut scratch));
  println!("{:?}", sort_into(&[1, 2, 3], &mut scratch));
  println!("{scratch:?}");

* Output

.. code:: text

  Ok([7, 42])
  Err(BufferTooSmall)
  [7, 42]

* Rejection leaves the previous buffer contents unchanged

---------------------------
Other Capacity Boundaries
---------------------------

.. code:: rust

  let sorted = bounded_sorted::<2>(&[42, 7]).unwrap();
  println!("{:?}", sorted.as_slice());
  println!("{}", bounded_sorted::<2>(&[1, 2, 3]).is_err());
  println!("{}", maximum_report::<16>(42).unwrap());
  println!("{}", maximum_report::<4>(42).is_err());

* Output

.. code:: text

  [7, 42]
  true
  Maximum: 42
  true

* Capacity errors can be observed without exhausting system memory

----------------------------------
Compare the Library Configurations
----------------------------------

* Three supported crate configurations

.. code:: sh

  cargo check --lib --no-default-features
  cargo check --lib --no-default-features --features alloc
  cargo check --lib

* Bounded APIs are available in all three configurations

* :rust:`try_sorted_samples` requires :rust:`alloc`

  * Enabled by default through :rust:`std`
  * A :rust:`no_std` application also needs an allocator

.. note::

  Successful compilation does not establish memory or timing bounds
