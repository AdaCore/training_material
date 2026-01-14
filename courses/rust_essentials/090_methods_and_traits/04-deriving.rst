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
    - Explicit copying
  * - :rust:`Copy`
    - Implicit bitwise copying
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
      // Default trait adds `default` constructor.

      let mut emp2 = emp1.clone();
      // Clone trait adds `clone` method.

      emp2.name = String::from("EldurScrollz");
      println!("{emp1:?} vs. {emp2:?}");
      // Debug trait adds support for printing with `{:?}`.
  }

* Compiler generates implementations
* Works if all fields also implement the trait
* Zero runtime cost

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


