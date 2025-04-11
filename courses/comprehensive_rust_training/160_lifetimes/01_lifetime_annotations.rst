======================
Lifetime Annotations
======================

----------------------
Lifetime Annotations
----------------------

A reference has a *lifetime*, which must not :dfn:`outlive` the value it
refers to. This is verified by the borrow checker.

The lifetime can be implicit - this is what we have seen so far.
Lifetimes can also be explicit: :rust:`&'a Point`, :rust:`&'document str`.
Lifetimes start with :rust:`'` and :rust:`'a` is a typical default name. Read
:rust:`&'a Point` as "a borrowed :rust:`Point` which is valid for at least the
lifetime :rust:`a`".

Lifetimes are always inferred by the compiler: you cannot assign a
lifetime yourself. Explicit lifetime annotations create constraints
where there is ambiguity; the compiler verifies that there is a valid
solution.

Lifetimes become more complicated when considering passing values to and
returning values from functions.

.. code:: rust
   :number-lines: 1

   #[derive(Debug)]
   struct Point(i32, i32);

   fn left_most(p1: &Point, p2: &Point) -> &Point {
       if p1.0 < p2.0 {
           p1
       } else {
           p2
       }
   }

   fn main() {
       let p1: Point = Point(10, 10);
       let p2: Point = Point(20, 20);
       let p3 = left_most(&p1, &p2); // What is the lifetime of p3?
       println!("p3: {p3:?}");
   }

.. warning::

   This code generates a compile error!

-----------------------------
Adding Lifetime Annotations
-----------------------------

In this example, the compiler does not know what lifetime to infer for
:rust:`p3`. Looking inside the function body shows that it can only safely
assume that :rust:`p3` lifetime is the shorter of :rust:`p1` and :rust:`p2`. But
just like types, Rust requires explicit annotations of lifetimes on
function arguments and return values.

Add :rust:`'a` appropriately to :rust:`left_most`:

.. container:: latex_environment small

   .. code:: rust
      :number-lines: 4

      fn left_most<'a>(p1: &'a Point, p2: &'a Point) -> &'a Point {

This says, "given p1 and p2 which both outlive :rust:`'a`, the return value
lives for at least :rust:`'a`."

In common cases, lifetimes can be elided, as described next
