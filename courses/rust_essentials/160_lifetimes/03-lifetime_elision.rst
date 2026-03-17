=============================
Lifetimes in Function Calls
=============================

----------------------------------
Lifetimes in Function Signatures
----------------------------------

- Lifetimes for function arguments and return values must be fully specified

.. code:: rust

  // Returned reference comes from 'slice'
  fn first<'a>(slice: &'a [i32]) -> &'a i32 {
      &slice[0]
  }

------------------
Lifetime Elision
------------------

- Most Rust code does **not** write lifetimes explicitly

- :dfn:`Lifetime elision` rules can be applied

- Essentially, syntactic shorthand (not inference)

.. code:: rust

  fn length(s: &str) -> usize

  // ...becomes

  fn length<'a>(s: &'a str) -> usize

-  Each reference parameter gets its own lifetime

-  If there is only one input lifetime, it is used for the return value

-  If the first parameter is :rust:`self`, that lifetime is used
   