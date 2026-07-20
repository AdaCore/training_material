===================
Safe / Unsafe Split
===================

---------------------------
Safe Rust and Unsafe Rust
---------------------------

Rust has a safe subset and an unsafe subset

.. list-table::
  :header-rows: 1
  :stub-columns: 1

  * - **Feature**
    - **Safe Rust**
    - **Unsafe Rust**

  * - **Memory Safety**
    - Enforced by compiler
    - Safety invariant required

  * - **Usage**
    - Default
    - Entered with :rust:`unsafe`

  * - **Capabilities**
    - Standard operations
    - Five extra operations

------------------
Five Superpowers
------------------

The :rust:`unsafe` keyword permits five additional operations

#. Dereference a raw pointer
#. Call an unsafe function or method
#. Access or modify a mutable static variable
#. Implement an unsafe trait
#. Access a field of a union

.. note::

  These operations require safety guarantees that Rust cannot verify.

---------------------------
What "unsafe" Does Not Do
---------------------------

* Does **not** disable type checking
* Does **not** turn off the borrow checker for references

  * Reference lifetimes are still checked

* Does **not** disable checks in surrounding Safe Rust
* Does **not** mean the code is necessarily incorrect

  * Some safety requirements cannot be verified mechanically

.. note::

  Unsafe Rust remains subject to normal language checks outside these five operations.

--------------------
The "unsafe" Block
--------------------

Place unsafe operations inside an unsafe block

.. code:: rust

  fn main() {
      let target_year = 1985;
      let flux_capacitor = &target_year as *const i32;

      // SAFETY: 'flux_capacitor' points to the live,
      // aligned local 'target_year'.
      unsafe {
          println!("Target year is: {}", *flux_capacitor);
      }
  }

.. note::

  Raw-pointer creation is safe, but dereferencing requires an unsafe block.
