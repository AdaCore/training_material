==============================
Lifetimes in Data Structures
==============================

------------------------------
Lifetimes in Data Structures
------------------------------

- If a struct stores references, it must specify a lifetime

.. code:: Rust

  .. container:: latex_environment scriptsize

    // 'a: lifetime of the referenced data (same as in functions)
    struct Highlight<'a>(&'a str);

- Instances of the struct cannot outlive the data they reference

------------------------
Borrowed vs Owned Data
------------------------

.. code:: Rust

  // Borrowed data
  struct Highlight<'a>(&'a str);

  // Owned data
  struct Highlight {
      text: String
  }

.. tip::

  Prefer owning data unless borrowing provides a clear benefit

--------------------------------
When to Use Explicit Lifetimes
--------------------------------

- Returning references from functions

- Structs store references

- Multiple input references create ambiguity

.. note::

    For most other cases, Rust's elision rules apply
