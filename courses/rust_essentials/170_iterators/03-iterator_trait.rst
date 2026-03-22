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

  * Type of value the iterator yields

* :rust:`next`

  * Called repeatedly to get the "next" value
  * Only required method
  * Returns :rust:`Option`

    * :rust:`Some(value)` - next element
    * :rust:`None` - iteration complete

----------------------------------------
Common Use Case - Iterating Over Slice
----------------------------------------

* Familiar pattern: iterating over a slice

  .. code:: rust

    for elem in [2, 4, 8, 16, 32] {
          println!("{}", elem);
      }

* This uses an iterator behind the scenes

  * :rust:`for` are syntactic sugar over :rust:`IntoIterator` trait

* Next, we'll look at how to build this ourselves

  * Create an iterator type
  * Implement the :rust:`Iterator` trait
  * Use them iterator in an example

-------------------------
Create an Iterator Type
-------------------------

.. code:: rust

  struct SliceIter<'s> {
      slice: &'s [i32],
      idx: usize,
  }

* :rust:`slice`

  * Data being iterated over

* :rust:`idx`

  * Current position in slice (our "state")

* Struct contains a reference, so referenced must be tied to its lifetime

  * Iterator holds a reference to the slice
  * :rust:`'s` ensures iterator cannot outlive data it references

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

  * Type being returned (:rust:`&i32`)
  * References value inside the slice

* :rust:`next` returns

  * :rust:`Some(next)` where :rust:`next` is the next element
  * :rust:`None` if :rust:`idx` index no more elements

.. tip::

  Just call :rust:`next()` until you run out of data!

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

  // The 'for' loop calls .next() under the hood
  for num in iter {
      println!("The number is: {}", num);
  }
