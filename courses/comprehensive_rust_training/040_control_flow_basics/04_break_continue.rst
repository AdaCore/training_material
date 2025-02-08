============================
"break" and "continue"
============================

----------------------------
"break" and "continue"
----------------------------

If you want to immediately start the next iteration use
`continue <https://doc.rust-lang.org/reference/expressions/loop-expr.html#continue-expressions>`__.

If you want to exit any kind of loop early, use
`break <https://doc.rust-lang.org/reference/expressions/loop-expr.html#break-expressions>`__.
With :rust:`loop`, this can take an optional expression that becomes the
value of the :rust:`loop` expression.

.. code:: rust

   fn main() {
       let mut i = 0;
       loop {
           i += 1;
           if i > 5 {
               break;
           }
           if i % 2 == 0 {
               continue;
           }
           println!("{}", i);
       }
   }

---------
Details
---------

Note that :rust:`loop` is the only looping construct which can return a
non-trivial value. This is because it's guaranteed to only return at a
:rust:`break` statement (unlike :rust:`while` and :rust:`for` loops, which can also
return when the condition fails).

--------
Labels
--------

Both :rust:`continue` and :rust:`break` can optionally take a label argument
which is used to break out of nested loops:

.. code:: rust

   fn main() {
       let s = [[5, 6, 7], [8, 9, 10], [21, 15, 32]];
       let mut elements_searched = 0;
       let target_value = 10;
       'outer: for i in 0..=2 {
           for j in 0..=2 {
               elements_searched += 1;
               if s[i][j] == target_value {
                   break 'outer;
               }
           }
       }
       print!("elements searched: {elements_searched}");
   }

---------
Details
---------

-  Labeled break also works on arbitrary blocks, e.g.

   .. code:: rust

      'label: {
          break 'label;
          println!("This line gets skipped");
      }

