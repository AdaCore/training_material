======================
Lifetime Annotations
======================

----------------------
Lifetime Annotations
----------------------

- Every reference has a lifetime

  - Usually, compiler determines it automatically

  - A :dfn:`lifetime annotation` can name it explicitly

- Written using a leading :rust:`'`

  .. code:: rust

    &'a
    &'my_life
    &'some_really_long_name

- **Example:** Reference to :rust:`str` valid for at least lifetime :rust:`'a`

  .. code:: rust
    
    &'a str

.. note::

  - Common to use short names (:rust:`'a`, :rust:`'b`, etc.) 
  
  - More descriptive names can be used (:rust:`'i_mean_something`)

--------------------------------------
Why Do We Need Lifetime Annotations?
--------------------------------------

- Describe relationships between references

  - Do not create *actual* lifetimes

- Only *required* when relationships are ambiguous

.. code:: rust

  // This code won't compile!
  fn choose(left: &str, right: &str) -> &str {
        if left.len() > right.len() { 
          left 
        } else {
          right
        }
    }

:error:`error[E0106]: missing lifetime specifier`

.. note::

  - Rust includes some predefined lifetimes

  - For this course, it's only important to know 
    
    - :rust:`'static` - means data lives for the entire program

    - :rust:`'_` - placeholder for an inferred lifetime

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
          left  // Return a reference to 'left'
      } else {
          right // Or return a reference to 'right'
      }
  }

.. note::

  Sometimes the term "lifetime" is used to indicate "lifetime annotation"

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
