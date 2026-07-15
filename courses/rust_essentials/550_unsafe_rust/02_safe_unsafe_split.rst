=========================
The Safe / Unsafe Split
=========================

---------------------------
Safe Rust and Unsafe Rust
---------------------------

Rust contains a safe subset and an unsafe subset

.. list-table::
  :header-rows: 1
  :stub-columns: 1

  * - **Feature**
    - **Safe Rust**
    - **Unsafe Rust**

  * - **Memory Safety**
    - Enforced by the language's static checks
    - Additional safety obligations are shifted to the programmer

  * - **Usage**
    - Default mode for all code
    - Explicitly isolated using the :rust:`unsafe` keyword

  * - **Capabilities**
    - Standard operations
    - Grants five specific "superpowers"

-------------------------------
The Five Unsafe Superpowers
-------------------------------

The :rust:`unsafe` keyword permits exactly five additional abilities

#. Dereference a raw pointer
#. Call an unsafe function or method
#. Access or modify a mutable static variable
#. Implement an unsafe trait
#. Access fields of a union

.. note::

  These operations are not automatically incorrect. The programmer must
  uphold the safety requirements that the compiler cannot verify.

--------------------------
What unsafe Does Not Do
--------------------------

* It does **not** turn off type checking
* It does **not** disable the borrow checker for references

  * References are still checked for valid lifetimes

* It does **not** disable safety checks in surrounding Safe Rust code
* It does **not** mean the code is necessarily dangerous or buggy

  * It means the compiler cannot mechanically verify every safety obligation

.. note::

  You are still writing Rust, not C. Normal language checks remain active
  outside of the five specific unsafe operations.

------------------
The unsafe Block
------------------

To perform an unsafe operation, isolate it inside an unsafe block

.. code:: rust

  fn main() {
      let num = 5;
      let r1 = &num as *const i32;

      // SAFETY: `r1` was created from a live, properly aligned reference.
      unsafe {
          println!("r1 is: {}", *r1);
      }
  }

.. note::

  Creating a raw pointer is safe. Dereferencing it is an unsafe operation.
