==================
Lifetime Elision
==================

----------------------------------
Why Are Lifetimes Often Omitted?
----------------------------------

- Writing lifetime (annotations) everywhere would be verbose

- Many lifetime patterns are predictable

- This would add unnecessary annotation in common cases

- Rust reduces this repetition automatically

------------------
Lifetime Elision
------------------

- Most Rust code does **not** write lifetimes explicitly

- :dfn:`Lifetime elision` rules can be applied

  - Compiler automatically assigns lifetimes using these rules

- Essentially, syntactic shorthand (not inference)

.. code:: rust

  fn length(s: &str) -> usize

  // ...becomes

  fn length<'a>(s: &'a str) -> usize

-  Each reference parameter gets its own lifetime

-  If there is only one input lifetime, it is used for the return value

-  If the first parameter is :rust:`self`, that lifetime is used

.. note::

  Even when not written, lifetimes are still present and assigned by the compiler
   