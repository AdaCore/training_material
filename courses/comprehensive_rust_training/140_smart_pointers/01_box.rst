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
