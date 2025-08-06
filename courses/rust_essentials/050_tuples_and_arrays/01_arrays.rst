========
Arrays
========

--------
Arrays
--------

.. code:: rust

   fn main() {
       let mut a: [i8; 10] = [42; 10];
       a[5] = 0;
       println!("a: {a:?}");
   }

---------
Details
---------

-  A value of the array type :rust:`[T; N]` holds :rust:`N` (a compile-time
   constant) elements of the same type :rust:`T`. Note that the length of
   the array is *part of its type*, which means that :rust:`[u8; 3]` and
   :rust:`[u8; 4]` are considered two different types. Slices, which have a
   size determined at runtime, are covered later.

-  Try accessing an out-of-bounds array element. Array accesses are
   checked at runtime. Rust can usually optimize these checks away, and
   they can be avoided using unsafe Rust.

-  We can use literals to assign values to arrays.

-  The :rust:`println!` macro asks for the debug implementation with the
   :rust:`?` format parameter: :rust:`{}` gives the default output, :rust:`{:?}`
   gives the debug output. Types such as integers and strings implement
   the default output, but arrays only implement the debug output. This
   means that we must use debug output here.

-  Adding :rust:`#`, eg :rust:`{a:#?}`, invokes a "pretty printing" format,
   which can be easier to read.
