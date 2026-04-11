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

  * But can be implemented manually

----------------------------
Basic Equality: Definition
----------------------------

**Rust splits equality into two traits**

* :rust:`PartialEq`

  * Enables :rust:`==` and :rust:`!=`
  * Symmetric: if :rust:`a == b` then :rust:`b == a`
  * Transitive: if :rust:`a == b` and :rust:`b == c` then :rust:`a == c`

  * Object are **considered** equal

    * Even if they are not identical
    * Think floating point numbers

* :rust:`Eq`

  * :dfn:`Marker trait` - no actual methods
  * Objects are **actually** equal

    * Trait guarantees that :rust:`a == a`

-----------------------------
Common Equality: Derivation
-----------------------------

**Most custom types do not need manual implementation**

* Use derivation when structs/enums should be compared normally

  .. code:: rust

    #[derive(PartialEq, Eq)]
    struct MyData {
      value_a: i32,
      value_b: i32,
    }

* :rust:`MyData` objects will be equal if all fields are equal

.. note::

  Can only derive if all fields already implement :rust:`PartialEq` and :rust:`Eq`

---------------------------------
Useful Equality: Implementation
---------------------------------

**Define your own method for dependent comparison**

* Example: Type has a validity flag and a value

  .. code:: rust

    struct SensorData {
      valid: bool,
      value: i32,
    }

* Objects are equal when

  * **EITHER** :rust:`valid` fields are :rust:`False`

    * Regardless of :rust:`value` contents

  * **OR** *all* fields are equal

    * :rust:`valid` is :rust:`True` and :rust:`value` matches

  .. code:: rust

    impl PartialEq for SensorData {
      fn eq(&self, other: &Self) -> bool {
        if self.valid && other.valid {
          self.value == other.value
        } else {
          self.valid == other.valid
        }
      }
    }

.. note::

  This is comparing the *concept* of "sensor data",
  not the equality of the object

----------
Ordering
----------

* Similar to :rust:`Eq` (and :rust:`PartialEq`)

  * Automatic implementation (:rust:`derive`) does a lexicographical comparison
  * User implementation allowed for **both** traits

* :rust:`Ord`

  * Build on :rust:`PartialOrd`
  * Returns :rust:`Ordering`

    :rust:`enum Ordering {Less, Equal, Greater,};`

* When using :rust:`derive`, Rust compares fields *in order*

  * So changing list of fields can change :rust:`Ord` result

.. note::

  :rust:`PartialEq` must be defined (either manual or derived)

------------------
Partial Ordering
------------------

:rust:`PartialOrd`

* Returns :rust:`Option<Ordering>`

  * :rust:`None` can be returned if two values cannot be compared

.. code:: rust
  :font-size: small

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

-----------------------
Ordering and Equality
-----------------------

* :rust:`PartialOrd` and :rust:`PartialEq` are *linked traits*

  * They must be **consistent**

* If :rust:`PartialOrd` returns :rust:`Ordering::Equal` for two objects

  * :rust:`PartialEq` **must** return :rust:`True` for those objects

.. warning::

  If :rust:`PartialOrd` and :rust:`PartialEq` are inconsistent,
  sorted collections (and even :rust:`<=` or :rust:`>=`) behavior
  will be inconsistent

