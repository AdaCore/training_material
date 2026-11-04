=================
Safe vs. Unsafe
=================

------------------
Why Unsafe Rust?
------------------

**Some operations require capabilities that Safe Rust forbids**

* An unsafe *context* permits those operations
* *Programmer* must uphold safety requirements the compiler cannot verify
* Other language rules still apply

.. note::

 Typical uses are interoperability and hardware access

------------------
Five Superpowers
------------------

#. Dereference a raw pointer
#. Call an unsafe function or method
#. Access or modify a mutable static item
#. Implement an unsafe trait
#. Read a field of a union

.. note::

  Rust has other unsafe features beyond these

---------------------------
What "unsafe" Does Not Do
---------------------------

* Does **not** disable type checking
* Does **not** turn off the borrow checker for references

  * Reference lifetimes are still checked

* Does **not** disable checks in surrounding Safe Rust
* Does **not** mean code is dangerous

  * Compiler cannot verify safety requirements

--------------------
The "unsafe" Block
--------------------

**Permits operations whose safety the compiler cannot verify**

.. code:: rust

  unsafe {
      // Unsafe operations go here
  }

* Programmer must ensure safety requirements are satisfied
* Violating these requirements can cause **undefined behavior**
* :rust:`unsafe` does not make undefined behavior acceptable

.. note::

    Unlike a panic, undefined behavior has no defined outcome
