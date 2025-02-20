=====================
Interior Mutability
=====================

---------------------
Interior Mutability
---------------------

In some situations, it's necessary to modify data behind a shared
(read-only) reference. For example, a shared data structure might have
an internal cache, and wish to update that cache from read-only methods.

The "interior mutability" pattern allows exclusive (mutable) access
behind a shared reference. The standard library provides several ways to
do this, all while still ensuring safety, typically by performing a
runtime check.

---------
Details
---------

The main thing to take away from this slide is that Rust provides *safe*
ways to modify data behind a shared reference. There are a variety of
ways to ensure that safety, and the next sub-slides present a few of
them.

--------------
:rust:`Cell"
--------------

:rust:`Cell` wraps a value and allows getting or setting the value using
only a shared reference to the :rust:`Cell`. However, it does not allow any
references to the inner value. Since there are no references, borrowing
rules cannot be broken.

.. code:: rust

   use std::cell::Cell;

   fn main() {
       // Note that `cell` is NOT declared as mutable.
       let cell = Cell::new(5);

       cell.set(123);
       println!("{}", cell.get());
   }

----------------------
:rust:`Cell` Details
----------------------

-  :rust:`Cell` is a simple means to ensure safety: it has a :rust:`set` method
   that takes :rust:`&self`. This needs no runtime check, but requires
   moving values, which can have its own cost.

-----------------
:rust:`RefCell`
-----------------

:rust:`RefCell` allows accessing and mutating a wrapped value by providing
alternative types :rust:`Ref` and :rust:`RefMut` that emulate :rust:`&T`/:rust:`&mut T`
without actually being Rust references.

These types perform dynamic checks using a counter in the :rust:`RefCell` to
prevent existence of a :rust:`RefMut` alongside another :rust:`Ref`/:rust:`RefMut`.

By implementing :rust:`Deref` (and :rust:`DerefMut` for :rust:`RefMut`), these types
allow calling methods on the inner value without allowing references to
escape.

.. code:: rust

   use std::cell::RefCell;

   fn main() {
       // Note that `cell` is NOT declared as mutable.
       let cell = RefCell::new(5);

       {
           let mut cell_ref = cell.borrow_mut();
           *cell_ref = 123;

           // This triggers an error at runtime.
           // let other = cell.borrow();
           // println!("{}", *other);
       }

       println!("{cell:?}");
   }

-------------------------
:rust:`RefCell` Details
-------------------------

-  :rust:`RefCell` enforces Rust's usual borrowing rules (either multiple
   shared references or a single exclusive reference) with a runtime
   check. In this case, all borrows are very short and never overlap, so
   the checks always succeed.

-  The extra block in the example is to end the borrow created by the
   call to :rust:`borrow_mut` before we print the cell. Trying to print a
   borrowed :rust:`RefCell` just shows the message :rust:`"{borrowed}"`.

-----------------
More to Explore
-----------------

There are also :rust:`OnceCell` and :rust:`OnceLock`, which allow initialization
on first use. Making these useful requires some more knowledge than
students have at this time.

