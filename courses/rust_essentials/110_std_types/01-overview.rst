==========
Overview
==========

-----------------------
Rust Standard Library
-----------------------

* Provides core types and APIs used across most programs

* Using common types helps libraries work together smoothly

  * Examples: :rust:`String` and :rust:`Vec`

* Is not a monolithic block

  * Layered stack designed to scale from microcontrollers to servers

* **core** (foundation)

  * Dependency-free base
  * Used in :rust:`#![no_std]` environments (embedded, kernels)

* **alloc** (middle layer)

  * Adds smart pointers and collections requiing dynamic allocation
  * Used when you have a heap but no OS

* **std** (full suite)

  * Complete library for general purpose programming
  * Default for desktop/server applications running on Linux, Windows, or macOS

--------------------------
One Library, Three Tiers
--------------------------

.. image:: rust_essentials/std_crate.svg

* Standard library is not a monolithic block

  * Layered stack designed to scale

* **core** (foundation)

  * Does not require an operating system or memory allocator
  * Contains basic types, primitive operations, macros

* **alloc** (middle layer)

  * Contains things like :rust:`Vec`, :rust:`String`, :rust:`Box`, etc

* **std** (full suite)

  * Contains everything in core and alloc, plus OS abstractions like file I/O, networking, etc



