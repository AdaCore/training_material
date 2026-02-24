==========
Overview
==========

--------------------------
One Library, Three Tiers
--------------------------

.. image:: rust_essentials/std_crate.svg
  :width: 80%

* Standard library is not a monolithic block

  * Layered stack designed to scale

* **core** (foundation)

  * Does not require an operating system or memory allocator
  * Contains basic types, primitive operations, macros

* **alloc** (middle layer)

  * Contains things like :rust:`Vec`, :rust:`String`, :rust:`Box`, etc

* **std** (full suite)

  * Contains everything in core and alloc, plus OS abstractions like file I/O, networking, etc



