==============
Introduction
==============

----------------
Topics Covered
----------------

* **Programming With** :rust:`core`

  - Familiar language features and :rust:`core` APIs remain available


* **Flexible Allocation**

  - Heap-backed types from :rust:`alloc`, with feature-gated APIs


* **Dependencies and Features**

  - Cargo features support multiple library configurations


* **Checking Configurations**

  - Host configuration checks and target-specific compilation

---------------------------
Why Not Always Use "std"?
---------------------------

* Not every target platform supports :rust:`std`

* Files, sockets, and OS threads depend on platform services
  that many bare-metal targets lack

* Libraries can avoid :rust:`std` to support a wider range of targets

----------------------
Three Library Layers
----------------------

**Library layers provide progressively more capabilities**

.. image:: rust_essentials/510_three_library_layers.svg
   :width: 100%
   :align: center

* :rust:`no_std` libraries can use :rust:`core` alone or with :rust:`alloc`

* :rust:`alloc` does not provide files, sockets, or OS threads

-----------------------
What "no_std" Changes
-----------------------

**Crate-level** :rust:`#![no_std]` **removes the automatic dependency on** :rust:`std`

.. code:: rust

  #![no_std]

* :rust:`#![...]` is an inner attribute, applying to the enclosing crate here

* :rust:`std` is no longer linked automatically for this crate

* :rust:`core` prelude replaces :rust:`std` prelude

  * :rust:`Option`, :rust:`Result`, and :rust:`Iterator` remain in scope

* Normal type checking and borrowing rules still apply

.. warning::

  Attribute does not forbid explicitly linking :rust:`std` or prevent dependencies from using it

--------------------------------
A Library Is Not an Executable
--------------------------------

* Library crates provide functions without starting the program

* :rust:`std` applications can depend on :rust:`no_std` libraries
