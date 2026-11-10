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

----------------------
Three Library Layers
----------------------

**Library layers provide progressively more capabilities**

.. image:: rust_essentials/510_three_library_layers.svg
   :width: 100%
   :align: center

* :rust:`no_std` libraries can use :rust:`core` alone or with :rust:`alloc`
* :rust:`alloc` does not provide files, sockets, or OS threads

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

.. note::

  :rust:`#![...]` is an inner attribute, applying to the enclosing crate here
