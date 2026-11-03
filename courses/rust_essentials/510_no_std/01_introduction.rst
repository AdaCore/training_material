==============
Introduction
==============

----------------
Topics Covered
----------------

* **Understanding** :rust:`no_std`

  - Effects of :rust:`no_std` and its intended use cases


* **Programming with** :rust:`core`

  - Familiar language features and :rust:`core` APIs remain available


* **Optional Allocation**

  - Optional heap-backed types through :rust:`alloc`


* **Dependencies and Features**

  - Cargo features support multiple library configurations


* **Checking Configurations**

  - Host configuration checks and target-specific compilation

---------------------------
Why Not Always Use "std"?
---------------------------

* :rust:`std` needs an implementation for the target platform

* Many of its services depend on platform support

  * Files, sockets, and OS threads need more than language support

* Many embedded and similar environments lack those services

* Libraries may also avoid :rust:`std` to reduce platform requirements

.. note::

  An embedded target can support :rust:`std`. A bare-metal program can have an allocator. These are *separate* capabilities

----------------------
Three Library Layers
----------------------

.. list-table::
  :header-rows: 1
  :widths: 20 80

  * - Library
    - What it provides
  * - :rust:`core`
    - Foundational types, traits, algorithms, and formatting
  * - :rust:`alloc`
    - Heap-backed types such as :rust:`Vec`, :rust:`String`, and :rust:`Box`
  * - :rust:`std`
    - Re-exports from these libraries, plus platform services

* :rust:`no_std` libraries can use :rust:`core` alone or together with :rust:`alloc`

* Using :rust:`alloc` does not enable files, sockets, or OS threads

.. note::

  These are library capabilities, not different versions of the Rust language

----------------------------
Library Layers at a Glance
----------------------------

.. image:: rust_essentials/510_three_library_layers.svg
   :width: 100%
   :align: center

* :rust:`core` is the base layer for every :rust:`no_std` crate

* :rust:`alloc` adds heap-backed types such as :rust:`Vec` and :rust:`String`

* :rust:`std` adds platform services such as files, sockets, and threads

------------------------
What "#![no_std]" Does
------------------------

**Configure the crate with an inner attribute at the top of** :filename:`src/lib.rs`

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

* Final program supplies the required runtime support

  * This includes panic handling and, when used, allocation support
