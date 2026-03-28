======================
Lifetime Annotations
======================

----------------------
Lifetime Annotations
----------------------

- Every reference has a lifetime

  - Usually, the compiler determines it automatically

  - A :dfn:`lifetime annotation` can name it explicitly

- Written using a leading :rust:`'`

.. code:: rust

  // Reference to 'str' valid for at least lifetime ''a'
  &'a str

.. info::

  Common to use short names (:rust:`'a`, :rust:`'b`, etc., but more
  descriptive names can be used)

--------------------------------------
Why Do We Need Lifetime Annotations?
--------------------------------------

- Describe relationships between references

  - Do not create *actual* lifetimes

  - Most lifetimes are inferred automatically by the compiler

- Only *required* when relationships are ambiguous

.. code:: rust

  fn choose(left: &str, right: &str) -> &str {
        if left.len() > right.len() { 
            left 
        } else {
          right
        }
    }

:command:`error[E0106]: missing lifetime specifier`

----------------------------------
Solving Ambiguity with Lifetimes
----------------------------------

**Previously, we encountered an error: a lifetime annotation solves it**

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
