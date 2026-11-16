==============
Introduction
==============

----------------
Topics Covered
----------------

* **Programming With** :rust:`core`

  - Familiar language features remain available
  - :rust:`core` APIs remain available

* **Flexible Allocation**

  - Heap-backed types from :rust:`alloc`
  - Allocation support supplied by the final program

* **Dependencies and Features**

  - Dependencies compatible with targets without :rust:`std`
  - :toolname:`Cargo` features select available library capabilities

* **Checking Configurations**

  - Host checks for supported feature configurations
  - Compilation checks for targets without :rust:`std`

----------------------
Three Library Layers
----------------------

**Library layers provide progressively more capabilities**

.. image:: rust_essentials/510_three_library_layers.svg
   :width: 100%
   :align: center

* :rust:`no_std` libraries can use :rust:`core` alone or with :rust:`alloc`

---------------------------
Why Not Always Use "std"?
---------------------------

* :rust:`std` provides file, socket, and OS-thread APIs

  * Many bare-metal targets lack the required platform services

* Libraries can avoid depending on :rust:`std`

  * This broadens the targets they can support

-----------------------
What "no_std" Changes
-----------------------

**Crate-level** :rust:`#![no_std]` **removes the automatic dependency on** :rust:`std`

.. code:: rust

  #![no_std]

* :rust:`core` prelude replaces :rust:`std` prelude

  * :rust:`Option`, :rust:`Result`, and :rust:`Iterator` remain in scope

* :rust:`#![no_std]` is an inner attribute applying to the entire crate

  * Place it at the beginning of the crate root
