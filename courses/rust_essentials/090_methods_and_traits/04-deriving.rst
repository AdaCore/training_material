==========
Deriving
==========

-----------------
Deriving Traits
-----------------

What does :dfn:`deriving` mean?

  * Automatically generates trait implementations
  * Uses :rust:`#[derive(...)]` attribute
  * Saves boilerplate for common behaviors

-------------------------
Commonly Derived Traits
-------------------------

.. list-table::
  :header-rows: 1

  * - Trait
    - Purpose

  * - :rust:`Debug`
    - Enables :rust:`{:?}` formatting
  * - :rust:`Clone`
    - Deep copy (everything it contains)
  * - :rust:`Copy`
    - Shallow copy (just the bits)
  * - :rust:`PartialEq, Eq`
    - Equality comparisons
  * - :rust:`PartialOrd, Ord`
    - Ordering
  * - :rust:`Hash`
    - Hash map / set keys
  * - :rust:`Default`
    - Default value construction

---------------------
Example of Deriving
---------------------

.. code:: rust

  #[derive(Debug, Clone, Default)]
  struct Employee {
      name: String,
      age: u8,
  }

  fn main() {
      let emp1 = Employee::default();
      // Default trait adds "default" constructor

      let mut emp2 = emp1.clone();
      // Clone trait adds "clone" method

      emp2.name = String::from("EldurScrollz");
      println!("{emp1:?} vs. {emp2:?}");
      // Debug trait adds support for printing with "{:?}"
  }

* Compiler generates implementations
* Works if all fields also implement the trait
* Zero runtime cost

--------------------------------
Deriving in Complex Structures
--------------------------------

* When a type derives a trait, its included elements must also derive the trait

  .. code:: rust

    struct Child {
        x: i32,
    }

    #[derive(Clone)]
    struct Parent {
        child: Child,
    }

.. container:: latex_environment scriptsize

  :error:`error[E0277]: the trait bound "main::Child: Clone" is not satisfied`

.. note::

  This is a general rule. There are traits that do not depend on fields
  which may allow you to skip this rule.

-------------
Orphan Rule
-------------

* You can implement a trait for a type **only** if you own *either* the Trait *or* the Type

  * Prevents "code breakage" where two libraries try to define the same behavior for the same type

    * Promotes global Consistency

* You **cannot** implement a foreign trait (like :rust:`Display`) for a foreign type (like :rust:`Vec`).

* Examples

  * Own the type not the trait

  * Own the trait not the type

    .. code:: rust

      impl 


  * Don't own either

    .. code:: rust

      impl Display for Vec<i32> {
          fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
              write!(f, "My vector: {:?}", self)
          }
      }

    :error:`error[E0117]: only traits defined in the current crate can be implemented for types defined outside of the crate`


-------------------------
Limitations on Deriving
-------------------------

You cannot derive when:

  * Behavior depends on logic, not structure
  * You need validation or side effects
  * Only part of the data should participate

.. note::

  In these cases, use a manual :rust:`impl Trait for Type` instead

---------------------------
"Derive" vs Manual "Impl"
---------------------------

.. list-table::
  :header-rows: 1

  * - Aspect
    - :rust:`derive`
    - Manual :rust:`impl`

  * - Effort
    - Minimal
    - More code

  * - Custom behavior
    - :color-red:`X`
    - :math:`\textcolor{green!65!black}{\checkmark}`

  * - Compile-time
    - :math:`\textcolor{green!65!black}{\checkmark}`
    - :math:`\textcolor{green!65!black}{\checkmark}`

  * - Readability
    - High
    - Depends


