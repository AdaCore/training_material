===============================
Exercise: Generic :rust:`min`
===============================

-------------------------------
Generic :rust:`min` Program
-------------------------------

In this short exercise, you will implement a generic :rust:`min` function
that determines the minimum of two values, using the
:url:`Ord <https://doc.rust-lang.org/stable/std/cmp/trait.Ord.html>`
trait.

.. code:: rust

   use std::cmp::Ordering;

   // TODO: implement the `min` function used in `main`.

   fn main() {
       assert_eq!(min(0, 10), 0);
       assert_eq!(min(500, 123), 123);

       assert_eq!(min('a', 'z'), 'a');
       assert_eq!(min('7', '1'), '1');

       assert_eq!(min("hello", "goodbye"), "goodbye");
       assert_eq!(min("bat", "armadillo"), "armadillo");
   }

-------------------------------
Generic :rust:`min` Solution
-------------------------------

.. code:: rust

   fn min<T: Ord>(l: T, r: T) -> T {
       match l.cmp(&r) {
           Ordering::Less | Ordering::Equal => l,
           Ordering::Greater => r,
       }
   }

------------------
More Information
------------------

:url:`Ord trait <https://doc.rust-lang.org/stable/std/cmp/trait.Ord.html>`

:url:`Ordering enum <https://doc.rust-lang.org/stable/std/cmp/enum.Ordering.html>`
