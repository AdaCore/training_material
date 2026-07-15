==================
Safe Abstractions
==================

--------------------------------
Encapsulating Unsafe Operations
--------------------------------

The core philosophy of Unsafe Rust is **encapsulation**

* Keep unsafe operations small and auditable
* Enforce every required check and invariant at the boundary
* Expose a safe API when the abstraction can guarantee safe use

  * Callers do not need their own unsafe blocks

* When a safe wrapper cannot enforce the contract, expose an unsafe API and
  document its :rust:`# Safety` requirements

The standard library uses this pattern throughout types such as
:rust:`Vec<T>` and :rust:`String`.

-------------------------------------
Why split_at_mut Needs Unsafe Rust
-------------------------------------

:rust:`split_at_mut` divides one mutable slice into two disjoint mutable slices

* The caller supplies a split index
* A bounds check guarantees :rust:`mid <= len`
* The two returned ranges do not overlap
* The implementation uses raw pointers because the borrow checker cannot prove
  that disjointness from indexing alone

.. code:: rust

  fn split_at_mut(
      values: &mut [i32],
      mid: usize,
  ) -> (&mut [i32], &mut [i32]) {
      // Implementation on the next slide
      todo!()
  }

-----------------------------
A Safe split_at_mut Wrapper
-----------------------------

.. code:: rust

  use std::slice;

  fn split_at_mut(
      values: &mut [i32],
      mid: usize,
  ) -> (&mut [i32], &mut [i32]) {
      let len = values.len();
      let ptr = values.as_mut_ptr();

      assert!(mid <= len);

      // SAFETY: `ptr` comes from `values`, `mid <= len`, and the two
      // constructed ranges are within one allocation and do not overlap.
      unsafe {
          (
              slice::from_raw_parts_mut(ptr, mid),
              slice::from_raw_parts_mut(ptr.add(mid), len - mid),
          )
      }
  }

.. note::

  The function is safe to call because its implementation checks the bound
  and preserves the exclusive-borrow invariant.
