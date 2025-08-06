==============
Introduction
==============

------------------------
Design Goals for SPARK
------------------------

* Same goals as any formal verification process: deep, sound, precise, fast, modular, constructive

* Combine tool automation and user interaction

  - Automate as much as possible
  - Rely on the user to provide essential code annotations

* Combine execution and proof of specifications
* Support the largest possible subset of Ada 2022

---------------------
Excluding Ambiguity
---------------------

* Soundness requires that program semantics are **clear**
* Easiest way is to avoid **language** ambiguities:

  - No :dfn:`erroneous behavior` from Ada Reference Manual

    - Cases where error can't be detected by the compiler or at run-time: e.g.
      dereference a pointer after it was deallocated

  - No :dfn:`unspecified` features from Ada Reference Manual

    - Cases where the compiler makes a choice: e.g. order of evaluation of
      parameters in a call

  - Limit :dfn:`implementation-defined` features from Ada Reference Manual

    - Cases where the choice of the compiler is documented: e.g. size of
      standard integer types

    - Analyzer should make the same choices as the compiler

* Also facilitates **portability** across platforms and compilers!

------------------------
SPARK Reference Manual
------------------------

* Precise definition of the SPARK subset
* Builds on the Ada Reference Manual

  - Follows the **same section numbering**
  - Has similar subsections:

    + **Syntax**
    + **Name Resolution Rules**
    + **Legality Rules**
    + **Static Semantics**
    + **Dynamic Semantics**
    + **Verification Rules** *(specific to SPARK RM)*
    + **Examples**

:url:`https://docs.adacore.com/live/wave/spark2014/html/spark2014_rm/packages.html`

