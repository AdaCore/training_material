======================
Lifetime Annotations
======================

----------------------
Lifetime Annotations
----------------------

- Rust can name lifetimes explicitly

.. code:: rust

  // Reference to 'str' valid for at least lifetime ''a'
  &'a str

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

- Most lifetimes are inferred automatically by the compiler

- Annotations are only *required* when relationships are ambiguous

.. code:: rust

  // The return value could come from either input,
  // so they must share the same lifetime
  fn choose<'a>(left: &'a str, right: &'a str) -> &'a 

----------------------------------
Lifetimes in Function Signatures
----------------------------------

- Lifetimes appear in reference types

- They describe relationships between inputs and outputs

.. code:: rust

  // Returned reference comes from 'slice'
  fn first<'a>(slice: &'a [i32]) -> &'a i32 {
      &slice[0]
  }
