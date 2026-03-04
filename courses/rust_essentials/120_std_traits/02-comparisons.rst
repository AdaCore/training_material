=============
Comparisons
=============

-------------------
Comparison Traits
-------------------

* Provide a standard way for defining how types interact with comparison operators

  * **Equality:** :rust:`PartialEq`, :rust:`Eq`

    * Evaluate if two values are the same

  * **Ordering:** :rust:`PartialOrd`, :rust:`Ord`

    * Determine which value is "bigger"

  * Basis of Rust’s comparison operators like :rust:`==`, :rust:`<`, etc.

* Compiler and Standard Library use these traits in many APIs (e.g., sorting)
* Typically created via :rust:`#[derive]`

  * Not user-written

---------------------------
Equality (and Inequality)
---------------------------

* :rust:`PartialEq` - manual implementation

  * Used to compare two objects
  * Allows for :rust:`Object == Object` to be *false*

.. container:: latex_environment tiny

  .. code:: rust

     struct SensorData {
         valid: bool,
         value: i32,
     }

     impl PartialEq for MyData {
         fn eq(&self, other: &Self) -> bool {
             // If both are valid, compare their values if self.valid && other.valid {
                 self.value == other.value
             } else {
                 // Otherwise compare if validity is the same
                 self.valid == other.valid
             }
         }
     }

* :rust:`Eq` - automatic implementation

  * Guarantees that :rust:`Object == Object` will always be *true*
  * So :rust:`derive` is used to perform a *lexicographical comparison*

    * Means it uses the "expected" definition of equality

.. container:: latex_environment tiny

  .. code:: rust
    :font-size: tiny

    #[derive(PartialEq, Eq)]
    struct MyData {
        value_a: i32,
        value_b: i32,
    }

.. note::

  :rust:`Eq` is a **marker** trait, meaning there are no actual methods.

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

    :rust:`(Less, Greater, or Equal)`

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

