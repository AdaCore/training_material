=============
Comparisons
=============

-------------------
Comparison Traits
-------------------

* Mechanism to define how types interact with comparison operators

  * **Equality:** :rust:`PartialEq`, :rust:`Eq`

    * Evaluate if two values are the same

  * **Ordering:** :rust:`PartialOrd`, :rust:`Ord`

    * Determine which value is "bigger"

  * Basis of Rust’s comparison operators like :rust:`==`, :rust:`<`, etc.

* Compiler and Standard Library use these traits in many APIs

  * Sorting, matching, etc.

* Typically created via :rust:`#[derive]`

  * Not user-written

--------------------
(Partial) Equality
--------------------

* **Partial Equality**

  * Treat the objects as *equivalent*
  * Even if parts of the object are not *equal*

* :rust:`PartialEq` 

  * Manual implementation
  * Coder decides what *equal* means

.. container:: latex_environment tiny

  .. code:: rust

     struct SensorData {
         valid: bool,
         value: i32,
     }

     impl PartialEq for SensorData {
         fn eq(&self, other: &Self) -> bool {
             // If both are valid, compare their values
             if self.valid && other.valid {
                 self.value == other.value
             } else {
                 // Otherwise compare if validity is the same
                 self.valid == other.valid
             }
         }
     }

-----------------
Actual Equality
-----------------

* :rust:`Eq` - automatic implementation

  * All parts of the object are equal

  * :rust:`derive` is used to perform *field-by-field comparison*

    * Uses the "expected" definition of equality

.. container:: latex_environment tiny

  .. code:: rust
    :font-size: tiny

    #[derive(PartialEq, Eq)]
    struct MyData {
        value_a: i32,
        value_b: i32,
    }

.. note::

  :rust:`Eq` is a **marker** trait, meaning there are no actual methods

----------
Ordering
----------

* Similar to :rust:`PartialEq` / :rust:`Eq`

  * Automatic implementation (:rust:`derive`) does a lexicographical comparison
  * Manual implementation requires multiple traits

* :rust:`Ord`

  * Can be called by :rust:`PartialOrd` (or vice-versa)
  * :rust:`PartialEq` must be defined (either manual or derived)
  * Returns :rust:`Ordering`

    :rust:`enum Ordering {Less, Equal, Greater}`;

* :rust:`PartialOrd`

  * Returns :rust:`Option<Ordering>`

    * :rust:`None` can be returned if two values cannot be compared

.. code:: rust

  use std::cmp::Ordering;

  impl PartialOrd for MyData {
      fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
          // If one is invalid and the other isn't, the valid one is "greater"
          match (self.valid, other.valid) {
              (true, true) => self.value.partial_cmp(&other.value),
              (true, false) => Some(Ordering::Greater),
              (false, true) => Some(Ordering::Less),
              (false, false) => Some(Ordering::Equal), 
          }
      }
  }

