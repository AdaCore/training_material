=================================
:rust:`Iterator` Helper Methods
=================================

---------------------------------
:rust:`Iterator` Helper Methods
---------------------------------

In addition to the :rust:`next` method that defines how an iterator behaves,
the :rust:`Iterator` trait provides 70+ helper methods that can be used to
build customized iterators.

.. code:: rust

   let result: i32 = (1..=10) // Create a range from 1 to 10
       .filter(|&x| x % 2 == 0) // Keep only even numbers
       .map(|x| x * x) // Square each number
       .sum(); // Sum up all the squared numbers

   println!("The sum of squares of even numbers from 1 to 10 is: {}", result);

---------
Details
---------

-  The :rust:`Iterator` trait implements many common functional programming
   operations over collections (e.g. :rust:`map`, :rust:`filter`, :rust:`reduce`,
   etc). This is the trait where you can find all the documentation
   about them.

-  Many of these helper methods take the original iterator and produce a
   new iterator with different behavior. These are know as "iterator
   adapter methods".

-  Some methods, like :rust:`sum` and :rust:`count`, consume the iterator and
   pull all of the elements out of it.

-  These methods are designed to be chained together so that it's easy
   to build a custom iterator that does exactly what you need.

-----------------
More to Explore
-----------------

-  Rust's iterators are extremely efficient and highly optimizable. Even
   complex iterators made by combining many adapter methods will still
   result in code as efficient as equivalent imperative implementations.
