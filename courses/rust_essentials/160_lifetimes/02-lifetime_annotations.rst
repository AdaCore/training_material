======================
Lifetime Annotations
======================

---------------------
What is a Lifetime?
---------------------

A lifetime is the span of code during which a reference is valid

.. code:: rust

  // 'second' must not outlive 'first'
  let first = 10;
  let second = &first;
  println!("{second}");

----------------------
Why Lifetimes Matter
----------------------

- References point to data owned elsewhere

- If data disappears while a reference still exists...

  - ...the program would have a **dangling reference**

- Rust ensures that a reference doesn't outlive the value it refers to

  - This rule is enforced at **compile time**

----------------------------------
Lifetimes and the Borrow Checker
----------------------------------

- The borrow checker verifies lifetime relationships

  - Reference Lifetime <= Value Lifetime

- This prevents

  - Dangling References

  - Use-after-free

  - Invalid memory access

- No garbage collector required!

----------------------
Lifetime Annotations
----------------------

- Rust can name lifetimes explicitly

.. code:: rust

  &'a str // Reference to 'str' valid for at least lifetime ''a'

- Lifetimes start with :rust:`'`

.. code:: rust

  // Examples of names:
  'some_name
  'a_lifetime
  'existence

------------------------------
What Lifetime Annotations Do
------------------------------

- Lifetime annotations do not create lifetimes

- They describe relationships between References

.. code:: rust

  // 'a' and 'b' must live at least ''a'
  // The returned rerference also lives at least ''a'
  fn choose<'a>(a: &'a str, b: &'a str) -> &'a str






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

---------
Details
---------

In this example, the compiler does not know what lifetime to infer for
:rust:`p3`. Looking inside the function body shows that it can only safely
assume that :rust:`p3` lifetime is the shorter of :rust:`p1` and :rust:`p2`. But
just like types, Rust requires explicit annotations of lifetimes on
function arguments and return values.

Add :rust:`'a` appropriately to :rust:`left_most`:

.. code:: rust

   fn left_most<'a>(p1: &'a Point, p2: &'a Point) -> &'a Point {

This says, "given p1 and p2 which both outlive :rust:`'a`, the return value
lives for at least :rust:`'a`."

In common cases, lifetimes can be elided, as described on the next
slide.
