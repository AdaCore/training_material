====================
Generic Data Types
====================

--------------------
Generic Data Types
--------------------

You can use generics to abstract over the concrete field type:

.. code:: rust

   #[derive(Debug)]
   struct Point<T> {
       x: T,
       y: T,
   }

   impl<T> Point<T> {
       fn coords(&self) -> (&T, &T) {
           (&self.x, &self.y)
       }

       fn set_x(&mut self, x: T) {
           self.x = x;
       }
   }

   fn main() {
       let integer = Point { x: 5, y: 10 };
       let float = Point { x: 1.0, y: 4.0 };
       println!("{integer:?} and {float:?}");
       println!("coords: {:?}", integer.coords());
   }

---------
Details
---------

-  *Q:* Why :rust:`T` is specified twice in :rust:`impl<T> Point<T> {}`? Isn't
   that redundant?

   -  This is because it is a generic implementation section for generic
      type. They are independently generic.
   -  It means these methods are defined for any :rust:`T`.
   -  It is possible to write :rust:`impl Point<u32> { .. }`.

      -  :rust:`Point` is still generic and you can use :rust:`Point<f64>`, but
         methods in this block will only be available for
         :rust:`Point<u32>`.

-  Try declaring a new variable :rust:`let p = Point { x: 5, y: 10.0 };`.
   Update the code to allow points that have elements of different
   types, by using two type variables, e.g., :rust:`T` and :rust:`U`.
