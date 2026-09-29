===================
Safe Abstractions
===================

---------------------------------
Encapsulating Unsafe Operations
---------------------------------

**Safe abstractions can encapsulate unsafe operations**

* Keep unsafe operations small and easy to audit
* Check the required safety conditions at the API boundary
* Expose a safe API when callers cannot violate those conditions

  * Callers need no :rust:`unsafe` block

* Otherwise, expose an unsafe API and document its :rust:`# Safety` contract

* Examples: :rust:`Vec<T>`, :rust:`String`, and many standard-library types

-------------------------------------------
Example: Safely Splitting a Mutable Slice
-------------------------------------------

:rust:`split_at_mut` **creates two disjoint mutable slices from one slice**

* Caller supplies the split index
* We must verify :rust:`mid <= len`
* The original slice provides valid, aligned storage
* The two returned ranges must stay in-bounds and must not overlap
* Raw pointers express a split the borrow checker cannot prove

------------------------------------
Establishing the Safety Conditions
------------------------------------

.. code:: rust

  use std::slice;

  fn split_at_mut(
      values: &mut [i32],
      mid: usize,
  ) -> (&mut [i32], &mut [i32]) {
      let len = values.len();

      // Start from valid slice storage
      let ptr = values.as_mut_ptr();

      // Keep both ranges in-bounds
      assert!(mid <= len);

      // Continue on the next slide
      todo!()
  }

---------------------------------
Creating the Two Mutable Slices
---------------------------------

**Now the checked raw pointer can be used to build two disjoint slices**

.. code:: rust

  // SAFETY:
  // - both ranges stay within the original slice
  // - the two ranges are disjoint
  unsafe {
      (
          slice::from_raw_parts_mut(ptr, mid),
          slice::from_raw_parts_mut(ptr.add(mid), len - mid),
      )
  }

.. note::

  This completes the :rust:`split_at_mut` implementation from the previous slide
