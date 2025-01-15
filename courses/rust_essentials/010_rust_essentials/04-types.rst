=======
Types
=======

---------------
Numeric Types
---------------

* Set of built-in types:

    - Integer types: :rust:`i8`, :rust:`i16`, :rust:`i32`, :rust:`i64`, :rust:`i128`
    - Unsigned types: :rust:`u8`, :rust:`u16`, :rust:`u32`, :rust:`u64`, :rust:`u128`

* No way to define custom integer types
* Statically/strongly typed
* Two floating point types: :rust:`f32` and :rust:`f64`

--------------------
Other Scalar Types
--------------------

* Boolean: Named :rust:`bool`, either :rust:`true` or :rust:`false`. Not an enum!
* Character: Named :rust:`char`, can be any valid Unicode value.
* All in all, less powerful than Ada, but also much simpler.

-------------------
Overflow Checking
-------------------

* In debug builds: raises an error
* In release builds: wrap around
* Heritage of C++'s zero-cost abstraction mentality

------------
Tuple Type
------------

* Most basic composite type
* Anonymous collection of elements.
* Structurally typed

.. code:: Rust

   fn main() {
       let tp = (1, 2)
       //  ^ Type of this is (i32, i32)

       let (x, y) = tp;
       //  ^ This is an irrefutable pattern

       let f = tp.1;
       // Access first value of tuple
   }

------------
Array Type
------------

* Homogeneous array type
* Index type is usize
* Bounds checked
* Very simple (dare I say primitive). No variable length arrays at all.
* 90% of the time one will use vectors

.. code:: Rust

   fn main() {
       let a = [1, 2, 3, 4, 5];

       println!("{}", a[4]);
}
   
---------
Vectors
---------

* As we said before, arrays in Rust are mostly useless
* In most cases you'll want to use vectors (:rust:`Vec<T>`)
* Vectors can be variable size, and are growable, *but*, they're always heap
  allocated

.. code:: Rust

   fn main() {
       let mut a = [1, 2, 3, 4].to_vec();
       //                      ^ Transform an array or slice into a vector

       let b = vec![1, 2, 3, 4];
       // Same thing as above

       let c = vec![1; 100];
       // Vector of 100 elements, all "1"

       println!("{:?}", a);
       //         ^ Print vector via the Debug trait
       //         If you can't print something, try this

       a.push(5);
       println!("{:?}", a);
   }

--------
Slices
--------

* Slices are a bit like arrays, but they just a view into a sequence. The type is written :rust:`[T]`, but is not used directly, but rather through pointers.

.. code:: Rust

   fn main() {
       let a = [1, 2, 3, 4, 5, 6, 7];
       let mut v = vec![1, 2, 3, 4, 5, 6, 7];

       let b = &a[1 .. 3];
       //      ^ Reference to a view of items 1 to 3 of the array a

       let c = &v[3 .. 5];
       //       ^ Reference to a view of items 3 to 5 of the vec v

       println!("{:?}", c);
       // By some ownership magic, after this statement, the lifetime of the
       // reference c is over

       v.clear();

       println!("{:?}", b);
   }

---------
Strings
---------

There are two main string types in Rust

* :rust:`String` is similar to a :rust:`Vec<u8>`, except:

    - It always points to a valid utf-8 sequence
    - You cannot index it

* :rust:`str` is a slice type. It is always used through a reference (:rust:`&str`)

* An array of characters is *not* a :rust:`String`

.. code:: Rust

   fn main() {
       let message: &str = "Hello world";

       for c in message.chars() {
           print!("{}", c);
       }
       println!("");
   }

