==============
Introduction
==============

-------------------------
Programming by Contract
-------------------------

* Pioneered by programming language **Eiffel** in the 80's

  - Since then adopted in Ada, .NET
  - Also being discussed for C++, Rust
  - Available as libraries for many languages

* The :dfn:`contract` of a subprogram defines:

  - What a caller guarantees to the subprogram (the precondition)
  - What the subprogram guarantees to its caller (the postcondition)

* A contract should include **all** the necessary information

  - Completes the API
  - Caller should **not** rely on **implementation details**
  - Typically parts of the contract are in English

--------------------
Contracts in SPARK
--------------------

* Preconditions and postconditions added in Ada 2012

  - Using the aspect syntax for :ada:`Pre` and :ada:`Post`
  - Already in GNAT since 2008 as pragmas

* Language support goes much **beyond** contracts-as-a-library

  - Ability to relate pre-state and post-state with attribute :ada:`Old`
  - **Fine-grained** control over execution

    .. code:: ada

       pragma Assertion_Policy (Pre => Check);
       pragma Assertion_Policy (Post => Ignore);

* :toolname:`GNATprove` analysis based on contracts

  - Precondition should be sufficient to prove subprogram **itself**
  - Postcondition should be sufficient to prove **its callers**
  - ...at all levels of software assurance beyond Bronze!

* SPARK contracts by cases, for callbacks, for OOP, etc.

