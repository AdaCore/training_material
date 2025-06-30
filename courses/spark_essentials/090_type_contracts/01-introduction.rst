==============
Introduction
==============

-------------------
Range Constraints
-------------------

.. admonition:: Language Variant

   Ada 83

* Scalar ranges gives **tighter** bounds to scalar types

  - Integer types: signed, modular
  - Real types: floating-point, fixed-point

  .. code:: ada

     type Nat is range 0 .. Integer'Last;
     type Nat is new Integer range 0 .. Integer'Last;
     subtype Nat is Integer range 0 .. Integer'Last;

* Also in standard subtypes :ada:`Natural` and :ada:`Positive`
* Range constraint also for enumeration and array types

  .. code:: ada

     subtype Week_Day is Day range Monday .. Friday;

     type Index is range 1 .. 100;
     type Table is array (Index range <>) of Integer;
     subtype Table_10 is Table (1 .. 10);

--------------------------
Discriminant Constraints
--------------------------

.. admonition:: Language Variant

   Ada 83

* Record discriminants can be **specialized** to specific values
* Formal bounded containers from SPARK Library

  .. code:: ada

     type Vector (Capacity : Capacity_Range) is record ...
     My_Vec : Vector (10);

* Discriminant without default cannot be changed

  - Needs to be defined at variable declaration

* Discriminant with default can be changed

  - If variable :ada:`Var` declared with unconstrained type
  - Then :ada:`Var'Constrained = False`

-----------------------
Richer Type Contracts
-----------------------

.. admonition:: Language Variant

   Ada 2012

* Predicates and invariants added in Ada 2012

  - Using the aspect syntax for :ada:`Predicate` and :ada:`Type_Invariant`

|

* Language support goes **much beyond** contracts-as-a-library

  - Constraint expressed once and verified *everywhere*
  - Fine-grain control over execution

    .. code:: ada

       pragma Assertion_Policy (Predicate => Check);
       pragma Assertion_Policy (Type_Invariant => Ignore);

|

* :toolname:`GNATprove` analysis based on contracts

  - Predicates and invariants assumed on subprogram inputs
  - Predicates and invariants proved on subprogram outputs
  - ...at all levels of software assurance beyond Bronze!

