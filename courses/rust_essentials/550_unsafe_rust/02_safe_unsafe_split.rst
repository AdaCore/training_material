=================
Safe vs. Unsafe
=================

---------------------------
Safe Rust and Unsafe Rust
---------------------------

**Unsafe Rust permits specific operations that Safe Rust forbids**

* Safe Rust does not allow unsafe operations
* An unsafe context permits those operations
* The programmer must uphold safety requirements Rust cannot verify
* Other Rust language rules still apply

.. note::

  :rust:`unsafe` does not mean unchecked code

------------------
Five Superpowers
------------------

**This module focuses on five classic unsafe operations**

#. Dereference a raw pointer
#. Call an unsafe function or method
#. Access or modify a mutable static item
#. Implement an unsafe trait
#. Read a field of a union

.. note::

  Rust has other unsafe features beyond these five

---------------------------
What "unsafe" Does Not Do
---------------------------

* Does **not** disable type checking
* Does **not** turn off the borrow checker for references

  * Reference lifetimes are still checked

* Does **not** disable checks in surrounding Safe Rust
* Does **not** mean the code is necessarily incorrect

  * Some safety requirements cannot be verified mechanically

--------------------
The "unsafe" Block
--------------------

**An unsafe block marks where unsafe operations are permitted**

.. code:: rust

  unsafe {
      // Unsafe operations go here
  }

* :rust:`unsafe` does not make an operation safe by itself
* The programmer must uphold the operation's safety requirements

.. note::

  The next sections introduce the unsafe operations used inside these blocks
