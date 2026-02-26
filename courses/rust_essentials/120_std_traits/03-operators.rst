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

Define an implementation for :rust:`Inches` that returns a number in :rust:`Feet`

.. code:: rust

  struct Feet(f64);
  struct Inches(f64);

  impl std::ops::Add for Inches {
      type Output = Feet;

      fn add(self, rhs: Self) -> Self::Output {
          Feet((self.0 + rhs.0) / 12.0)
      }
  }

  fn main() {
      let measure1 = Inches(10.0);
      let measure2 = Inches(32.0);
    
      println!("{} inches + {} inches", measure1.0, measure2.0);
    
      let feet = measure1 + measure2;
      println!("= {} feet", feet.0);
  }
