==================
Lifetime Elision
==================

----------------------------------
Why Are Lifetimes Often Omitted?
----------------------------------

- Writing lifetime (annotations) everywhere would be verbose

- Many lifetime patterns are predictable

- Would add unnecessary annotation in common cases

.. code:: rust

  // Same lifetime repeated in multiple places
  fn print<'a>(s: &'a str) {
      println!("{s}");
  }

  // Input and output clearly share the same lifetime
  // (but we still have to write it everywhere)
  fn first<'a>(slice: &'a [i32]) -> &'a i32 {
      &slice[0]
  }

  // Lifetime doesn't even affect the return value
  // (but we still have to write it on the parameter)
  fn len<'a>(s: &'a str) -> usize {
      s.len()
  }

.. note::

  Rust reduces this repetition automatically

------------------
Lifetime Elision
------------------

- Most Rust code does **not** write lifetimes explicitly

- :dfn:`Lifetime elision` rules can be applied

  -  Each reference parameter gets its own lifetime

  -  If there is only one input lifetime, it is used for the return value

  -  If first parameter is :rust:`self`, that lifetime is used

- Compiler automatically assigns lifetimes using these rules

- Essentially, syntactic shorthand (not inference)

.. code:: rust

  fn length(s: &str) -> usize

  // ...is interpreted as

  fn length<'a>(s: &'a str) -> usize

.. note::

  Compiler infers lifetimes even when they're not written
  
