================
"IntoIterator"
================

--------------------------
The "IntoIterator" Trait
--------------------------

* Defines how type can be converted into an iterator

  * Basis of :rust:`for` loop

* Core method: :rust:`into_iter(self)`

* Takes :rust:`self` as a parameter

  * **Not** :rust:`&self`
  * So it **consumes** the collection

    * Original variable moved into iterator
    * Original variable can no longer be used

.. code:: rust

  pub trait IntoIterator {
      type Item;
      type IntoIter: Iterator<Item = Self::Item>;

      fn into_iter(self) -> Self::IntoIter;
  }

---------------------
Implicit Conversion
---------------------

* :rust:`for` loop does not actually loop over :rust:`Vec` or :rust:`Array`

  * Actually loops over an *iterator*
  * Rust uses :rust:`IntoIterator` to "convert"

* When you write

  .. code:: rust

    let v = vec![1, 2, 3];
    for x in v {
        println!("{x}");
    }

* Compiler sees

  .. code:: rust

    let v = vec![1, 2, 3];
    // The IntoIterator trait provides this!
    let mut iter = v.into_iter();
    while let Some(x) = iter.next() {
        println!("{x}");
    }

------------------------
Making a Type Loopable
------------------------

**Implement** :rust:`IntoIterator` **for your own collection**

.. code:: rust

  struct MyCollection {
      items: Vec<i32>,
  }

  impl IntoIterator for MyCollection {
      type Item = i32;
      type IntoIter = std::vec::IntoIter<i32>;

      fn into_iter(self) -> Self::IntoIter {
          self.items.into_iter()
      }
  }

  fn main() {
      let col = MyCollection { items: vec![10, 20] };
      for x in col { // This works because of the implementation above!
          println!("{x}");
      }
  }
