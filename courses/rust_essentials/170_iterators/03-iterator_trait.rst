================
Iterator Trait
================

------------------------------
Describing an Iterator Trait
------------------------------

.. code:: rust

   trait Iterator {
       type Item;
       fn next(&mut self) -> Option<Self::Item>;
   }

* :rust:`type Item;`

  * Associated type defines what iterator produces

* :rust:`next`

  * Only required method
  * Returns :rust:`Option`

    * :rust:`Some(value)` for next element in sequence
    * :rust:`None` when iteration complete

----------------------------------------
Common Use Case - Iterating Over Slice
----------------------------------------

* We know we can iterate over a slice

  .. code:: rust

    for elem in [2, 4, 8, 16, 32] {
          println!("{}", elem);
      }

* This is just Rust creating an "easy" iterator!

* The next few slides show how we would do this manually

  * Create an iterator type
  * Implement the iterator trait
  * Use our iterator in an example

-------------------------
Create in Iterator Type
-------------------------

.. code:: rust

  struct SliceIter<'s> {
      slice: &'s [i32],
      idx: usize,
  }

* :rust:`slice`

  * Array we will iterate over

* :rust:`idx`

  * Iteration state

.. note::

  We use the *lifetime annotation* to show it's safe to give a
  reference from the iterator

--------------------------
Implement Iterator Trait
--------------------------

.. code:: rust

  impl<'s> Iterator for SliceIter<'s> {
      type Item = &'s i32;
      fn next(&mut self) -> Option<Self::Item> {
          if self.idx == self.slice.len() {
              None
          } else {
              let next = &self.slice[self.idx];
              self.idx += 1;
              Some(next)
          }
      }
  }

* :rust:`Item`

  * Type being returned (:rust:`i32`)

* :rust:`next` returns

  * :rust:`None` if :rust:`idx` is the end of the slice
  * :rust:`Some(next)` where :rust:`next` is the next item

--------------------
Using Our Iterator 
--------------------

.. code:: rust

  let numbers = [10, 20, 30];
    
  // Create the iterator manually
  let mut iter = SliceIter {
      slice: &numbers,
      idx: 0,
  };

  // The 'for' loop calls .next() automatically
  for num in iter {
      println!("The number is: {}", num);
  }
