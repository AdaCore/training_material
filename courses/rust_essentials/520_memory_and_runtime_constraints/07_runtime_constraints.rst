=====================
Runtime Constraints
=====================

---------------------------------------
One Operation, Three Memory Contracts
---------------------------------------

**Sorting can use different storage and ownership strategies**

.. image:: rust_essentials/520_sorting_api_contracts.svg
   :width: 100%
   :align: center

* All three preserve the input and sort the same :rust:`u16` values

* Ownership, output storage, and failure behavior differ

-----------------------------------
Bound Memory, Then Bound the Work
-----------------------------------

* **Memory bounds**

  * Reject inputs exceeding supported size limits

  * Reuse storage when repeated allocation is unacceptable

* **Execution-time bounds**

  * Budget for comparisons, copying, sorting, cleanup, and synchronization

  * Measure and analyze costs on the deployment target

* Compilation alone establishes neither bound

.. warning::

  * :rust:`sort_unstable` time depends on slice length, not capacity

  * Avoiding heap allocation does not make sorting constant-time

-----------------------------------
Handle Memory and Capacity Limits
-----------------------------------

* **Reject:** return an error without returning a partial result

  * :rust:`unwrap()` panics on :rust:`Err` instead of returning the error

* **Degrade:** perform a smaller, explicitly supported operation

* **Drop data:** discard according to a documented application policy

* **Retry:** only for temporary resource shortages

  * Fixed-capacity errors require a larger buffer or smaller input

-----------------------------------
Capacity Limits: Observed Results
-----------------------------------

* Example using standard output on a host

.. code:: rust

  let mut scratch = [0_u16; 2];
  println!("{:?}", sort_into(&[42, 7], &mut scratch));
  println!("{:?}", sort_into(&[1, 2, 3], &mut scratch));
  println!("{scratch:?}");

* Output

.. code:: output

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

.. code:: output

  [7, 42]
  true
  Maximum: 42
  true

* Capacity errors can be observed without exhausting system memory

