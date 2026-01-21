=============
Comparisons
=============

-------------------
Comparison Traits
-------------------

* What are comparison traits?

  * Standard traits for comparing values

    * :rust:`PartialEq`
    * :rust:`Eq`
    * :rust:`PartialOrd`
    * :rust:`Ord`

  * Basis of Rust’s comparison operators like ==, <, etc.

* Why do we need them?

  * Compiler and standard library use these traits in many APIs (e.g., sorting)
  * Lets you define what it means for two values to be equal (or ordered)

    * Ignore some fields, or compare one field based on another

* Often derived automatically

---------------------------
Equality (and Inequality)
---------------------------

* :rust:`Eq`

  * Used for :rust:`==` and :rust:`!=`

* :rust:`PartialEq`

  * Used instead of :rust:`Eq` if the type contains values that are not comparable

    * Think :rust:`NaN` for a float object

.. code:: rust

  struct Key {
      id: u32,
      metadata: Option<String>,
  }
  impl PartialEq for Key {
      fn eq(&self, other: &Self) -> bool {
          self.id == other.id
      }
  }

----------
Ordering
----------

* :rust:`Ord`

  * Used for comparisons like :rust:`>=` and :rust:`<`

* :rust:`PartialOrd`

  * Used instead of :rust:`Ord` if the type contains values that are not comparable

    * Think :rust:`NaN` for a float object

.. code:: rust

  use std::cmp::Ordering;
  #[derive(Eq, PartialEq)]
  struct Citation {
      author: String,
      year: u32,
  }
  impl PartialOrd for Citation {
      fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
          match self.author.partial_cmp(&other.author) {
              Some(Ordering::Equal) => self.year.partial_cmp(&other.year),
              author_ord => author_ord,
          }
      }
  }
