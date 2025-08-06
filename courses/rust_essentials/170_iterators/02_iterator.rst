========================
:rust:`Iterator` Trait
========================

------------------------
:rust:`Iterator` Trait
------------------------

The
:url:`Iterator <https://doc.rust-lang.org/std/iter/trait.Iterator.html>`
trait defines how an object can be used to produce a sequence of values.
For example, if we wanted to create an iterator that can produce the
elements of a slice it might look something like this:

.. code:: rust

   struct SliceIter<'s> {
       slice: &'s [i32],
       i: usize,
   }

   impl<'s> Iterator for SliceIter<'s> {
       type Item = &'s i32;

       fn next(&mut self) -> Option<Self::Item> {
           if self.i == self.slice.len() {
               None
           } else {
               let next = &self.slice[self.i];
               self.i += 1;
               Some(next)
           }
       }
   }

   fn main() {
       let slice = &[2, 4, 6, 8];
       let iter = SliceIter { slice, i: 0 };
       for elem in iter {
           println!("elem: {elem}");
       }
   }

---------
Details
---------

-  The :rust:`SliceIter` example implements the same logic as the C-style
   :rust:`for` loop demonstrated on the last slide.

-  Point out to the students that iterators are lazy: Creating the
   iterator just initializes the struct but does not otherwise do any
   work. No work happens until the :rust:`next` method is called.

-  Iterators don't need to be finite! It's entirely valid to have an
   iterator that will produce values forever. For example, a half open
   range like :rust:`0..` will keep going until integer overflow occurs.

-----------------
More to Explore
-----------------

-  The "real" version of :rust:`SliceIter` is the
   :url:`slice::Iter <https://doc.rust-lang.org/stable/std/slice/struct.Iter.html>`
   type in the standard library, however the real version uses pointers
   under the hood instead of an index in order to eliminate bounds
   checks.

-  The :rust:`SliceIter` example is a good example of a struct that contains
   a reference and therefore uses lifetime annotations.

-  You can also demonstrate adding a generic parameter to :rust:`SliceIter`
   to allow it to work with any kind of slice (not just :rust:`&[i32]`).
