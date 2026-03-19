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

- Describe relationships between references

  - Do not create *actual* lifetimes

  - Most lifetimes are inferred automatically by the compiler

- Only *required* when relationships are ambiguous

.. code:: rust

  // The return value could come from either input,
  // so they must share the same lifetime
  fn choose<'a>(left: &'a str, right: &'a str) -> &'a str {
      // Return 'left' or 'right' depending on the condition
      if left.len() > right.len() {
          left  // return a reference to 'left'
      } else {
          right // or return a reference to 'right'
      }
  }

----------------------------------
Lifetimes in Function Signatures
----------------------------------

- Appear in reference types

- Describe relationships between inputs and outputs

.. code:: rust

  // Returned reference comes from 'slice'
  fn first<'a>(slice: &'a [i32]) -> &'a i32 {
      &slice[0]
  }

.. note::
  
  First element of a slice cannot live longer than the slice itself