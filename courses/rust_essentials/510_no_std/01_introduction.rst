==============
Introduction
==============

----------------
Topics Covered
----------------

* **Why** :rust:`no_std`?

  - Understand why :rust:`std` is not available in every environment


* **Programming with** :rust:`core`

  - Keep familiar Rust features, types, and algorithms


* **Optional Allocation**

  - Use :rust:`alloc` when heap-backed types are needed


* **Dependencies and Features**

  - Support different library configurations


* **Checking and Testing**

  - Validate the supported configurations on the development machine

---------------------------
Why Not Always Use "std"?
---------------------------

* :rust:`std` needs an implementation for the compilation target

* Many of its services depend on platform support

  * Files, sockets, and OS threads need more than language support

* Many embedded and other restricted environments lack that integration

* A library may also avoid :rust:`std` to reduce its platform requirements

.. note::

  An embedded target can support :rust:`std`. A bare-metal program can have an allocator. These are separate capabilities

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

* A :rust:`no_std` library can use :rust:`core` alone or also use :rust:`alloc`

* Using :rust:`alloc` does not restore files, sockets, or OS threads

.. tip::

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

**Place this crate-level attribute at the top of** :filename:`src/lib.rs`

.. code:: rust

  #![no_std]

* :rust:`std` is no longer linked automatically for this crate

* The :rust:`core` prelude replaces the :rust:`std` prelude

  * :rust:`Option`, :rust:`Result`, and :rust:`Iterator` remain in scope

* Normal Rust type checking and borrowing rules still apply

.. warning::

  This attribute does not forbid explicitly linking :rust:`std`, or prevent dependencies from using it

--------------------------------
A Library Is Not an Executable
--------------------------------

* Our library provides functions; it does not start the program

* A normal :rust:`std` application can depend on a :rust:`no_std` library

* The final program supplies the required runtime support

  * This includes panic handling and, when used, allocation support

.. note::

  A reusable library does not need its own :rust:`main`, :rust:`#![no_main]`, or :rust:`#[panic_handler]` just because it uses :rust:`no_std`
