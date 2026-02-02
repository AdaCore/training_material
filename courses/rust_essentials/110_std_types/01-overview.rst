==============
Overview
==============

-----------------------
Rust Standard Library
-----------------------

* Provides core types and APIs used across most programs

* Using common types helps libraries work together smoothly.

  * Examples: :rust:`String` and :rust:`Vec`

* Library layers

  * :rust:`core`

    * Minimal, no OS or heap (for embedded / low-level)

  * :rust:`alloc`

    * Adds heap allocation (Vec, Box, Arc)

  * :rust:`std`

    * Full library (I/O, threads, collections, networking)
