==========
Deriving
==========

-----------------
Deriving Traits
-----------------

* What **is** :dfn:`deriving`?

  * Built-in macro to automatically generate code

* What does **deriving** do?

  * Automatically generates trait implementations
  * Uses :rust:`#[derive(...)]` attribute
  * Saves boilerplate for common behaviors

-------------------------
Commonly Derived Traits
-------------------------

.. list-table::
  :header-rows: 1

  * - **Trait**
    - **Purpose**

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
    - Hash map/set keys
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
      let human = Employee::default();
      // Default trait adds 'default' constructor

      let mut smith = human.clone();
      // Clone trait adds 'clone' method

      smith.name = String::from("Agent Smith");
      println!("{human:?} vs. {smith:?}");
      // Debug trait adds support for printing with '{:?}'
  }

.. container:: latex_environment scriptsize

  :command:`Employee \{ name: "", age: 0 \} vs. Employee \{ name: "Agent Smith", age: 0 \}`

* Compiler generates implementations
* Works if all fields also implement the trait
* Zero runtime cost

--------------------------------
Deriving in Complex Structures
--------------------------------

**Deriving a trait generally requires its inner types to implement the trait**

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

-------------------------
Limitations on Deriving
-------------------------

**You cannot derive when**

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
    :stub-columns: 1

    * - **Decision Factor**
      - :rust:`#[derive(...)]`
      - **Manual** :rust:`impl`

    * - *Logic Source*
      - **Compiler-Generated**
      - **Programmer-Written**

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

.. note::

  **Derive** is for **Computers**

    If you just need the compiler to know how to clone your data or print it for a log, let it do the work

  **Manual** is for **Humans**

    If you are formatting a string that a programmer will read (like :rust:`Display`), you usually need a manual implementation to make it look "pretty"
