=============================
Lifetimes in Function Calls
=============================

-----------------------------
Lifetimes in Function Calls
-----------------------------

Lifetimes for function arguments and return values must be fully
specified, but Rust allows lifetimes to be elided in most cases with
:url:`a few simple rules <https://doc.rust-lang.org/nomicon/lifetime-elision.html>`. This
is not inference - it is just a syntactic shorthand.

-  Each argument which does not have a lifetime annotation is given one.
-  If there is only one argument lifetime, it is given to all
   un-annotated return values.
-  If there are multiple argument lifetimes, but the first one is for
   :rust:`self`, that lifetime is given to all un-annotated return values.

.. code:: rust

   #[derive(Debug)]
   struct Point(i32, i32);

   fn cab_distance(p1: &Point, p2: &Point) -> i32 {
       (p1.0 - p2.0).abs() + (p1.1 - p2.1).abs()
   }

   fn nearest<'a>(points: &'a [Point], query: &Point) -> Option<&'a Point> {
       let mut nearest = None;
       for p in points {
           if let Some((_, nearest_dist)) = nearest {
               let dist = cab_distance(p, query);
               if dist < nearest_dist {
                   nearest = Some((p, dist));
               }
           } else {
               nearest = Some((p, cab_distance(p, query)));
           };
       }
       nearest.map(|(p, _)| p)
   }

   fn main() {
       let points = &[Point(1, 0), Point(1, 0), Point(-1, 0), Point(0, -1)];
       println!("{:?}", nearest(points, &Point(0, 2)));
   }

---------
Details
---------

In this example, :rust:`cab_distance` is trivially elided.

The :rust:`nearest` function provides another example of a function with
multiple references in its arguments that requires explicit annotation.

Try adjusting the signature to "lie" about the lifetimes returned:

.. code:: rust

   fn nearest<'a, 'q>(points: &'a [Point], query: &'q Point) -> Option<&'q Point> {

This won't compile, demonstrating that the annotations are checked for
validity by the compiler. Note that this is not the case for raw
pointers (unsafe), and this is a common source of errors with unsafe
Rust.

Students may ask when to use lifetimes. Rust borrows *always* have
lifetimes. Most of the time, elision and type inference mean these don't
need to be written out. In more complicated cases, lifetime annotations
can help resolve ambiguity. Often, especially when prototyping, it's
easier to just work with owned data by cloning values where necessary.
