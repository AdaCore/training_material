============
"Box<T>"
============

------------
"Box<T>"
------------

`Box <https://doc.rust-lang.org/std/boxed/struct.Box.html>`__ is an
owned pointer to data on the heap:

.. code:: rust

   fn main() {
       let five = Box::new(5);
       println!("five: {}", *five);
   }

:rust:`Box<T>` implements :rust:`Deref<Target = T>`, which means that you can
`call methods from T directly on a Box<T> <https://doc.rust-lang.org/std/ops/trait.Deref.html#more-on-deref-coercion>`__.

Recursive data types or data types with dynamic sizes cannot be stored
inline without a pointer indirection. :rust:`Box` accomplishes that
indirection:

.. code:: rust

   #[derive(Debug)]
   enum List<T> {
       /// A non-empty list: first element and the rest of the list.
       Element(T, Box<List<T>>),
       /// An empty list.
       Nil,
   }

   fn main() {
       let list: List<i32> =
           List::Element(1, Box::new(List::Element(2, Box::new(List::Nil))));
       println!("{list:?}");
   }
