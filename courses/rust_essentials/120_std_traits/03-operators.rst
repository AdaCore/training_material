===========
Operators
===========

----------------------
Operator Overloading
----------------------

* Trait-based operator overloading

  * Rust uses traits in std::ops to overload operators (e.g., :rust:`+`, :rust:`-`, :rust:`*`)
  * Operators delegate to trait methods (:rust:`Add`, :rust:`Sub`, etc.)

* Example

  * Implementing :rust:`Add` for your type allows **a + b** syntax
  * Requires associated type :rust:`Output` for the result type

---------
Example
---------

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

.. note::

  :rust:`Output` is an associated type — implementer decides the result type
