=====================
Smart Pointer Types
=====================

-----
Box
-----

Box is a simple reference. Used when you want to *store* a reference, rather than just *borrow* it (see the expression evaluator exercise).

.. code:: Rust

   fn main() {
       let b = Box::new(5);
       println!("b = {}", b);
   }

---------
Box (2)
---------

* You cannot have multiple references to a box!!

.. code:: Rust

   enum List {
       Cons(i32, Box<List>),
       Nil,
   }

   use crate::List::{Cons, Nil};

   fn main() {
       let a = Cons(5, Box::new(Cons(10, Box::new(Nil))));
       let b = Cons(3, Box::new(a));
       let c = Cons(4, Box::new(a));
   }

----
Rc
----

.. container:: latex_environment small

   .. code:: Rust

      enum List {
          Cons(i32, Rc<List>),
          Nil,
      }

      use crate::List::{Cons, Nil};
      use std::rc::Rc;

      fn main() {
          let a = Rc::new(Cons(5, Rc::new(Cons(10, Rc::new(Nil)))));
          let b = Cons(3, Rc::clone(&a));
          let c = Cons(4, Rc::clone(&a));
      }
