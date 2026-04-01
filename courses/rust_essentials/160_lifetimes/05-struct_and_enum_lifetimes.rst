================================
Lifetimes in Structs and Enums
================================

----------------------
Lifetimes in Structs
----------------------

- If a struct stores references, it **must** specify a lifetime

- Instances of the struct cannot outlive the data they reference

.. code:: Rust

  // Missing lifetime — does not compile
  struct Scroll {
      inscription: &str,
  }

:error:`error[E0106]: missing lifetime specifier`

.. code:: Rust

  // Lifetime ties the struct to the referenced data
  struct Scroll<'a> {
      inscription: &'a str,
  }

.. note::

  We refer to structs here, but enums behave the same way

-----------------------------
Borrowed Data vs Owned Data
-----------------------------

.. code:: Rust

  // Borrowed data (requires a lifetime)
  struct Scroll<'a> {
      inscription: &'a str,
  }

  // Owned data (no lifetime needed)
  struct Scroll {
      inscription: String,
  }

.. tip::

  Better to own data *unless* borrowing provides clear benefits

--------------------------------
When to Use Explicit Lifetimes
--------------------------------

- Returning references from functions

- Structs store references

- Multiple input references create ambiguity

.. note::

    For most other cases, lifetime elision rules apply
