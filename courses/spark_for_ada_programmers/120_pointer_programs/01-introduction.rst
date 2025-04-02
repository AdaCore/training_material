==============
Introduction
==============

--------------------------
Absence of Interferences
--------------------------

* Flow analysis rejects aliasing

  - Between two parameters
  - Between a parameter and a global variable
  - ... when that may lead to interferences

|

* Interferences when one of the variables is written

|

* Many features avoid direct use of pointers

  - Array types
  - By-reference parameter passing mode
  - Address specifications :ada:`X : Integer with Address => ...`
  - Generics (avoid C-style :code:`void*` genericity)

|

* What about pointers?

-----------------------
Pointers and Aliasing
-----------------------

* Pointers introduce aliasing

  - This violates SPARK principle of absence of interferences

|

* Rust programming language popularized :dfn:`ownership`

  - Only one pointer (the *owner*) at any time has read-write access
  - Assigning a pointer transfers its ownership

|

* Work on ownership in SPARK started in 2017

  - First version released in SPARK Pro 20
  - Detection of memory leaks in SPARK Pro 21
  - Support for all access types in SPARK Pro 22
  - SPARK libraries for aliasing in SPARK Pro 23

