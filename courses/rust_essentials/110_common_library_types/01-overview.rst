==========
Overview
==========

--------------------------
One Library, Three Tiers
--------------------------

.. image:: rust_essentials/std_crate.svg
  :width: 80%

* Standard Library is not a monolithic block

  * Layered stack designed to scale

* **core** (foundation)

  * No OS or memory allocator required
  * Basic types, primitive operations

* **alloc** (middle layer)

  * Depends on **core**
  * Requires heap allocator
  * Growable types (:rust:`Vec`, :rust:`String`, :rust:`Box`, etc.)

* **std** (full suite)

  * Requires host environment
  * Contains everything in **core** and **alloc**
  * Adds OS abstractions like file I/O, networking, etc.

----------------------
What is the Prelude?
----------------------

* Small collection of items automatically imported into every module

* Designed to reduce boilerplate for frequent tasks

* In this module, that means items dealing with

  * Logic: :rust:`Option` and :rust:`Result` (Error handling)
  * Data: :rust:`String` and :rust:`Vec` (Dynamic storage)

* Why it matters

  * Types so essential that they're needed in every file
