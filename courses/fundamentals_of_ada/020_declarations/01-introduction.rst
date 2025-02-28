==============
Introduction
==============

----------------
Ada Type Model
----------------

* Each :dfn:`object` is associated a :dfn:`type`
* **Static** Typing

   - Object type **cannot change**
   - ... but run-time polymorphism available (OOP)

* **Strong** Typing

   - **Compiler-enforced** operations and values
   - **Explicit** conversions for "related" types
   - **Unchecked** conversions possible

* Predefined types
* Application-specific types

    - User-defined
    - Checked at compilation and run-time

------------
Declarations
------------

* :dfn:`Declaration` associates a :dfn:`name` to an :dfn:`entity`

    - Objects
    - Types
    - Subprograms
    - et cetera

* In a :dfn:`declarative part`
* Example: :ada:`N : Type := Value;`

    - ``N`` is usually an :dfn:`identifier`

.. warning:: Declaration **must precede** use

* **Some** implicit declarations

    - **Standard** types and operations
    - **Implementation**-defined

