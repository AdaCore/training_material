==================
"IntoIterator"
==================

------------------
"IntoIterator"
------------------

The :rust:`Iterator` trait tells you how to *iterate* once you have created
an iterator. The related trait
:url:`IntoIterator <https://doc.rust-lang.org/std/iter/trait.IntoIterator.html>`
defines how to create an iterator for a type. It is used automatically
by the :rust:`for` loop.

.. code:: rust

   struct Grid {
       x_coords: Vec<u32>,
       y_coords: Vec<u32>,
   }

   impl IntoIterator for Grid {
       type Item = (u32, u32);
       type IntoIter = GridIter;
       fn into_iter(self) -> GridIter {
           GridIter { grid: self, i: 0, j: 0 }
       }
   }

   struct GridIter {
       grid: Grid,
       i: usize,
       j: usize,
   }

   impl Iterator for GridIter {
       type Item = (u32, u32);

       fn next(&mut self) -> Option<(u32, u32)> {
           if self.i >= self.grid.x_coords.len() {
               self.i = 0;
               self.j += 1;
               if self.j >= self.grid.y_coords.len() {
                   return None;
               }
           }
           let res = Some((self.grid.x_coords[self.i], self.grid.y_coords[self.j]));
           self.i += 1;
           res
       }
   }

   fn main() {
       let grid = Grid { x_coords: vec![3, 5, 7, 9], y_coords: vec![10, 20, 30, 40] };
       for (x, y) in grid {
           println!("point = {x}, {y}");
       }
   }

---------
Details
---------

-  :rust:`IntoIterator` is the trait that makes for loops work. It is
   implemented by collection types such as :rust:`Vec<T>` and references to
   them such as :rust:`&Vec<T>` and :rust:`&[T]`. Ranges also implement it. This
   is why you can iterate over a vector with
   :rust:`for i in some_vec { .. }` but :rust:`some_vec.next()` doesn't exist.

Click through to the docs for :rust:`IntoIterator`. Every implementation of
:rust:`IntoIterator` must declare two types:

-  :rust:`Item`: the type to iterate over, such as :rust:`i8`,
-  :rust:`IntoIter`: the :rust:`Iterator` type returned by the :rust:`into_iter`
   method.

Note that :rust:`IntoIter` and :rust:`Item` are linked: the iterator must have
the same :rust:`Item` type, which means that it returns :rust:`Option<Item>`

The example iterates over all combinations of x and y coordinates.

Try iterating over the grid twice in :rust:`main`. Why does this fail? Note
that :rust:`IntoIterator::into_iter` takes ownership of :rust:`self`.

Fix this issue by implementing :rust:`IntoIterator` for :rust:`&Grid` and
storing a reference to the :rust:`Grid` in :rust:`GridIter`.

The same problem can occur for standard library types:
:rust:`for e in some_vector` will take ownership of :rust:`some_vector` and
iterate over owned elements from that vector. Use
:rust:`for e in &some_vector` instead, to iterate over references to
elements of :rust:`some_vector`.
