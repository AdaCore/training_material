===================
Safe Abstractions
===================

---------------------------------
Encapsulating Unsafe Operations
---------------------------------

**Unsafe Rust relies on encapsulation**

* Keep unsafe operations small and easy to audit
* Enforce checks and invariants at the boundary
* Expose a safe API when safe use is guaranteed

  * Callers need no unsafe block

* Otherwise, expose an unsafe API and document its :rust:`# Safety` contract

The standard library applies this pattern throughout :rust:`Vec<T>`,
:rust:`String`, and many other types.

--------------------------------------
Why "split_at_mut" Needs Unsafe Rust
--------------------------------------

:rust:`split_at_mut` **creates two disjoint mutable slices from one slice**

* Caller supplies the split index
* Bounds check guarantees :rust:`mid <= len`
* Returned ranges do not overlap
* Raw pointers express a split indexing cannot prove

.. code:: rust

  fn split_at_mut(
      values: &mut [i32],
      mid: usize,
  ) -> (&mut [i32], &mut [i32]) {
      // Implementation on the next slide
      todo!()
  }

-------------------------------
A Safe "split_at_mut" Wrapper
-------------------------------

.. code:: rust

  use std::slice;

  fn split_at_mut(
      values: &mut [i32],
      mid: usize,
  ) -> (&mut [i32], &mut [i32]) {
      let len = values.len();
      let ptr = values.as_mut_ptr();
      assert!(mid <= len);

      // SAFETY: Both ranges are in-bounds and disjoint
      unsafe {
          (
              slice::from_raw_parts_mut(ptr, mid),
              slice::from_raw_parts_mut(ptr.add(mid), len - mid),
          )
      }
  }

.. note::

  The wrapper is safe because it returns two in-bounds, disjoint slices
