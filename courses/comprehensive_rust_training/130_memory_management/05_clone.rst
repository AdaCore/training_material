=======
Clone
=======

-------
Clone
-------

Sometimes you *want* to make a copy of a value. The :rust:`Clone` trait
accomplishes this.

.. code:: rust

   fn say_hello(name: String) {
       println!("Hello {name}")
   }

   fn main() {
       let name = String::from("Alice");
       say_hello(name.clone());
       say_hello(name);
   }

----------------
Why Use Clone?
----------------

-  The idea of :rust:`Clone` is to make it easy to spot where heap
   allocations are occurring. Look for :rust:`.clone()` and a few others
   like :rust:`vec!` or :rust:`Box::new`.

-  It's common to "clone your way out" of problems with the borrow
   checker, and return later to try to optimize those clones away.

-  :rust:`clone` generally performs a deep copy of the value, meaning that
   if you e.g. clone an array, all of the elements of the array are
   cloned as well.

-  The behavior for :rust:`clone` is user-defined, so it can perform custom
   cloning logic if needed.
