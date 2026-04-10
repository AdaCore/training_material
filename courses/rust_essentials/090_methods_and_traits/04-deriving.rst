==========
Deriving
==========

-----------------
Deriving Traits
-----------------

* What **is** :dfn:`deriving`?

  * Built-in macro to automatically generate code

* What does :dfn:`deriving` do?

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
      // Default trait adds 'default' constructor

      let mut emp2 = emp1.clone();
      // Clone trait adds 'clone' method

      emp2.name = String::from("EldurScrollz");
      println!("{emp1:?} vs. {emp2:?}");
      // Debug trait adds support for printing with '{:?}'
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

* You may implement a trait for a type only if you own the trait or the type

  * "Own" means: defined in your crate

* Why do we need this?

  * Prevents two libraries from defining conflicting behavior
  * Ensures trait implementations are globally unambiguous

* To implement trait :rust:`SomeTrait` for :rust:`SomeType`

  * You must own :rust:`SomeTrait` or :rust:`SomeType`
  * If you own neither |rightarrow| compile error

----------------------
Orphan Rule Examples
----------------------

**Own the type not the trait**

  .. code:: rust

    struct MyType(i32);      // Owned type
    impl Debug for MyType {} // External trait

**Own the trait not the type**

  .. code:: rust

    trait Hello { // Owned trait
        fn hello(&self) -> &'static str;
    }
    impl Hello for String { // External type
        fn hello(&self) -> &'static str {
            "Hello!"
        }
    }

**Don't own either**

  .. code:: rust

    impl Debug for Vec<i32> {}

.. container:: latex_environment tiny

  :error:`error[E0117]: only traits defined in the current crate can be implemented for types defined outside of the crate`

-------------------------
Limitations on Deriving
-------------------------

You cannot derive when

  * Behavior depends on logic, not structure
  * You need validation or side effects
  * Only part of the data should participate

.. note::

  In these cases, use a manual :rust:`impl Trait for Type` instead

----------------------------
"Derive" vs. Manual "Impl"
----------------------------

.. container:: latex_environment tiny

  .. list-table::
    :header-rows: 1

    * - Decision Factor
      - :rust:`#[derive(...)]`
      - Manual :rust:`impl`

    * - *Logic Source*
      - **Compiler-Generated**
      - **Programmer Written**

    * -
      - Uses standard "one-size-fits-all" template
      - Write code from scratch for total control

    * -
      -
      -

    * - *Effort*
      - **Minimal**
      - **High**
 
    * -
      - Single line above struct or enum
      - Reauires writing boilerplate and handling every field

    * -
      -
      -

    * - *Customization*
      - **None**
      - **Total**

    * -
      - It's "all-or-nothing" for every field in the struct
      - You can hide fields, transform data, skip logic

    * -
      -
      -

    * - *Compile-Time*
      - **Checked**
      - **Checked**

    * -
      - Compiler ensures logic is safe
      - Compiler ensures manual code is safe

  **Derive** is for **Computers**

    If you just need the compiler to know how to clone your data or print it for a log, let it do the work

  **Manual** is for **Humans**

    If you are formatting a string that a programmer will read (like :rust:`Display`), you usually need a manual implementation to make it look "pretty"
