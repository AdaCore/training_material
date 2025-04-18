=================
Borrow Checking
=================

-----------------
Borrow Checking
-----------------

Rust's *borrow checker* puts constraints on the ways you can borrow
values. We've already seen that a reference cannot *outlive* the value
it borrows:

.. code:: rust

   fn main() {
       let x_ref = {
           let x = 10;
           &x
       };
       println!("x: {x_ref}");
   }

There's also a second main rule that the borrow checker enforces: The
*aliasing* rule. For a given value, at any time:

-  You can have one or more shared references to the value, *or*
-  You can have exactly one exclusive reference to the value.

.. code:: rust

   fn main() {
       let mut a: i32 = 10;
       let b: &i32 = &a;

       {
           let c: &mut i32 = &mut a;
           *c = 20;
       }

       println!("a: {a}");
       println!("b: {b}");
   }

---------
Details
---------

-  The :dfn:`outlives` rule was demonstrated previously when we first looked
   at references. We review it here to show students that the borrow
   checking is following a few different rules to validate borrowing.
-  Note that the requirement is that conflicting references not *exist*
   at the same point. It does not matter where the reference is
   dereferenced.
-  The above code does not compile because :rust:`a` is borrowed as mutable
   (through :rust:`c`) and as immutable (through :rust:`b`) at the same time.
-  Move the :rust:`println!` statement for :rust:`b` before the scope that
   introduces :rust:`c` to make the code compile.
-  After that change, the compiler realizes that :rust:`b` is only ever used
   before the new mutable borrow of :rust:`a` through :rust:`c`. This is a
   feature of the borrow checker called :dfn:`non-lexical lifetimes`.
-  The exclusive reference constraint is quite strong. Rust uses it to
   ensure that data races do not occur. Rust also *relies* on this
   constraint to optimize code. For example, a value behind a shared
   reference can be safely cached in a register for the lifetime of that
   reference.
-  The borrow checker is designed to accommodate many common patterns,
   such as taking exclusive references to different fields in a struct
   at the same time. But, there are some situations where it doesn't
   quite "get it" and this often results in "fighting with the borrow
   checker."
