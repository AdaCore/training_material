===================
Shared References
===================

-------------------
Shared References
-------------------

A reference provides a way to access another value without taking
ownership of the value, and is also called :dfn:`borrowing`. Shared
references are read-only, and the referenced data cannot change.

.. code:: rust

   fn main() {
       let a = 'A';
       let b = 'B';
       let mut r: &char = &a;
       println!("r: {}", *r);
       r = &b;
       println!("r: {}", *r);
   }

A shared reference to a type :rust:`T` has type :rust:`&T`. A reference value is
made with the :rust:`&` operator. The :rust:`*` operator :dfn:`dereferences` a
reference, yielding its value.

------------------------------
More About Shared References
------------------------------

- References can never be null in Rust so null checking not necessary

- Reference **borrows** value it refers to

  - Code uses the reference to **access** the value
  - Original variable still **owns** the reference

- References are implemented as pointers

  - So they are usually much smaller than what they point to

- Rust does not automatically create references for you

  - :rust:`&` is always required.

- Rust will auto-dereference in some cases

  - In particular when invoking methods (e.g. :rust:`r.is_ascii()`)
  - No need for an :rust:`->` operator like in C++.

- Shared reference does not allow modifying the value it refers to

  - Even if that value was mutable

.. container:: speakernote

   - In this example, :rust:`r` is mutable so that it can be reassigned
     (:rust:`r = &b`). Note that this re-binds :rust:`r`, so that it refers to
     something else. This is different from C++, where assignment to a
     reference changes the referenced value.

   - Rust is tracking the lifetimes of all references to ensure they live
     long enough. Dangling references cannot occur in safe Rust.
     :rust:`x_axis` would return a reference to :rust:`point`, but :rust:`point` will
     be deallocated when the function returns, so this will not compile.
