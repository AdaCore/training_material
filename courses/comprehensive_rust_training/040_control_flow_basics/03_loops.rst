=======
Loops
=======

-------
Loops
-------

There are three looping keywords in Rust: :rust:`while`, :rust:`loop`, and
:rust:`for`:

-----------
"while"
-----------

The
`while keyword <https://doc.rust-lang.org/reference/expressions/loop-expr.html#predicate-loops>`__
works much like in other languages, executing the loop body as long as
the condition is true.

.. code:: rust

   fn main() {
       let mut x = 200;
       while x >= 10 {
           x = x / 2;
       }
       println!("Final x: {x}");
   }

-------------
"for"
-------------

The :rust:`for` `loop <https://doc.rust-lang.org/std/keyword.for.html>`__
iterates over ranges of values or the items in a collection:

.. code:: rust

   fn main() {
       for x in 1..5 {
           println!("x: {x}");
       }

       for elem in [1, 2, 3, 4, 5] {
           println!("elem: {elem}");
       }
   }

---------------
"for" Details
---------------

-  Under the hood :rust:`for` loops use a concept called :dfn:`iterators` to
   handle iterating over different kinds of ranges/collections.
   Iterators will be discussed in more detail later.
-  Note that the first :rust:`for` loop only iterates to :rust:`4`. Show the
   :rust:`1..=5` syntax for an inclusive range.

--------------
"loop"
--------------

The :rust:`loop`
`statement <https://doc.rust-lang.org/std/keyword.loop.html>`__ just
loops forever, until a :rust:`break`.

.. code:: rust

   fn main() {
       let mut i = 0;
       loop {
           i += 1;
           println!("{i}");
           if i > 100 {
               break;
           }
       }
   }

----------------
"loop" Details
----------------

-  The :rust:`loop` statement works like a :rust:`while true` loop. Use it for
   things like servers which will serve connections forever.
