==========
Deriving
==========

-----------------
Deriving Traits
-----------------

* What **is** :dfn:`deriving`?

  * Built-in macro to automatically generate code

What does :dfn:`deriving` do?

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

* **Can** implement trait for type **only** if you own the trait or the type

  * Prevents two libraries from defining same behavior for same type
  * To implement trait **Some_Trait** for **A_Type** you must own either **Some_Trait**
    or **A_Type**
  * So you **cannot** implement foreign trait :rust:`Debug` for foreign type :rust:`Vec`

**Examples**

*Own the type not the trait*

  .. code:: rust

    struct MyType(i32);      // owned type
    impl Debug for MyType {} // external trait

.. raw:: latex

  \vspace{1mm}

*Own the trait not the type*

  .. code:: rust

    trait Hello { // owned trait
        fn hello(&self) -> &'static str;
    }
    impl Hello for String { // external type
        fn hello(&self) -> &'static str {
            "Hello!"
        }
    }

.. raw:: latex

  \vspace{1mm}

*Don't own either*

  .. code:: rust

    impl Debug for Vec<i32> {}

  :color-red:`error[E0117]: only traits defined in the current crate can be implemented for types defined outside of the crate`

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


