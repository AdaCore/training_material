============
"Box"
============

------------
"Box"
------------

`Box <https://doc.rust-lang.org/std/boxed/struct.Box.html>`__ is an
owned pointer to data on the heap:

.. code:: rust

   fn main() {
       let five = Box::new(5);
       println!("five: {}", *five);
   }

:rust:`Box<T>` implements :rust:`Deref<Target = T>`, which means that you can
`call methods from T directly <https://doc.rust-lang.org/std/ops/trait.Deref.html#more-on-deref-coercion>`__.
on a :rust:`Box<T>`

