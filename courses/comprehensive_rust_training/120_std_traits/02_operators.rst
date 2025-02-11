===========
Operators
===========

-----------
Operators
-----------

Operator overloading is implemented via traits in
`std::ops <https://doc.rust-lang.org/std/ops/index.html>`__:

.. code:: rust

   #[derive(Debug, Copy, Clone)]
   struct Point {
       x: i32,
       y: i32,
   }

   impl std::ops::Add for Point {
       type Output = Self;

       fn add(self, other: Self) -> Self {
           Self { x: self.x + other.x, y: self.y + other.y }
       }
   }

   fn main() {
       let p1 = Point { x: 10, y: 20 };
       let p2 = Point { x: 100, y: 200 };
       println!("{p1:?} + {p2:?} = {:?}", p1 + p2);
   }

---------
Details
---------

Discussion points:

-  You could implement :rust:`Add` for :rust:`&Point`. In which situations is
   that useful?

   -  Answer: :rust:`Add:add` consumes :rust:`self`. If type :rust:`T` for which you
      are overloading the operator is not :rust:`Copy`, you should consider
      overloading the operator for :rust:`&T` as well. This avoids
      unnecessary cloning on the call site.

-  Why is :rust:`Output` an associated type? Could it be made a type
   parameter of the method?

   -  Short answer: Function type parameters are controlled by the
      caller, but associated types (like :rust:`Output`) are controlled by
      the implementer of a trait.

-  You could implement :rust:`Add` for two different types, e.g.
   :rust:`impl Add<(i32, i32)> for Point` would add a tuple to a :rust:`Point`.

The :rust:`Not` trait (:rust:`!` operator) is notable because it does not
"boolify" like the same operator in C-family languages; instead, for
integer types it negates each bit of the number, which arithmetically is
equivalent to subtracting it from -1: :rust:`!5 == -6`.
