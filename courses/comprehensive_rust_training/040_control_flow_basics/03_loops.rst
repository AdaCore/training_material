=======
Loops
=======

-------
Loops
-------

There are three looping keywords in Rust: :rust:`while`, :rust:`loop`, and
:rust:`for`:

-----------
:rust:`while`
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
