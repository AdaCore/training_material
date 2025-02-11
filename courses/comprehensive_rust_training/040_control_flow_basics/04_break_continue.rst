============================
``break`` and ``continue``
============================

----------------------------
``break`` and ``continue``
----------------------------

If you want to immediately start the next iteration use
:url:`continue <https://doc.rust-lang.org/reference/expressions/loop-expr.html#continue-expressions>`.

If you want to exit any kind of loop early, use
:url:`break <https://doc.rust-lang.org/reference/expressions/loop-expr.html#break-expressions>`.
With ``loop``, this can take an optional expression that becomes the
value of the ``loop`` expression.

.. code:: rust,editable

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

.. raw:: html

---------
Details
---------

Note that ``loop`` is the only looping construct which can return a
non-trivial value. This is because it's guaranteed to only return at a
``break`` statement (unlike ``while`` and ``for`` loops, which can also
return when the condition fails).

.. raw:: html

