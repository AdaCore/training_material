=========
"Vec"
=========

---------
"Vec"
---------

`Vec <https://doc.rust-lang.org/std/vec/struct.Vec.html>`__ is the
standard resizable heap-allocated buffer:

.. code:: rust

   fn main() {
       let mut v1 = Vec::new();
       v1.push(42);
       println!("v1: len = {}, capacity = {}", v1.len(), v1.capacity());

       let mut v2 = Vec::with_capacity(v1.len() + 1);
       v2.extend(v1.iter());
       v2.push(9999);
       println!("v2: len = {}, capacity = {}", v2.len(), v2.capacity());

       // Canonical macro to initialize a vector with elements.
       let mut v3 = vec![0, 0, 1, 2, 3, 4];

       // Retain only the even elements.
       v3.retain(|x| x % 2 == 0);
       println!("{v3:?}");

       // Remove consecutive duplicates.
       v3.dedup();
       println!("{v3:?}");
   }

:rust:`Vec` implements
:rust:`Deref<Target = [T]>`

.. 
   https://doc.rust-lang.org/std/vec/struct.Vec.html#deref-methods-%5BT%5D

which means that you can call slice methods on a :rust:`Vec`.

---------
Details
---------

-  :rust:`Vec` is a type of collection, along with :rust:`String` and
   :rust:`HashMap`. The data it contains is stored on the heap. This means
   the amount of data doesn't need to be known at compile time. It can
   grow or shrink at runtime.
-  Notice how :rust:`Vec<T>` is a generic type too, but you don't have to
   specify :rust:`T` explicitly. As always with Rust type inference, the
   :rust:`T` was established during the first :rust:`push` call.
-  :rust:`vec![...]` is a canonical macro to use instead of :rust:`Vec::new()`
   and it supports adding initial elements to the vector.
-  To index the vector you use :rust:`[` :rust:`]`, but they will panic if out
   of bounds. Alternatively, using :rust:`get` will return an :rust:`Option`.
   The :rust:`pop` function will remove the last element.
-  Slices are covered on day 3. For now, students only need to know that
   a value of type :rust:`Vec` gives access to all of the documented slice
   methods, too.
