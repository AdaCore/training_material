****************
Type Contracts
****************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

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

=================
Type Predicates
=================

---------------------------
What is a Type Predicate?
---------------------------

* Boolean property that should **always hold** for objects of the type

  - Name of the type used to refer to an object of the type
  - Direct use of component names also allowed

* Can be specified on a type or subtype

  .. code::Ada

     type Non_Zero is new Integer
       with Predicate => Non_Zero /= 0;

     subtype Even is Integer
       with Predicate => Even mod 2 = 0;

* Type predicate can be static or dynamic

  - Aspect :ada:`Predicate` can be :ada:`Static_Predicate` or :ada:`Dynamic_Predicate`

  .. code::Ada

     type Non_Zero is new Integer
       with Static_Predicate => Non_Zero /= 0;

     subtype Even is Integer
       with Dynamic_Predicate => Even mod 2 = 0;

* Like a type constraint, part of membership test :ada:`X in T`

-----------------------------
Static vs Dynamic Predicate
-----------------------------

* **Static** predicates are **more restricted**

  - Boolean combination of comparisons with **static** values
  - Usable mostly on scalar and enumeration types
  - That does **not** mean statically checked by the compiler

|

* **Dynamic** predicates are **arbitrary** boolean expressions

  - Applicable to array and record types

|

* Types with static predicates are allowed in more contexts

  - Used as range in a *for loop*
  - Used as choice in *case statement* or *case expression*

|

* Aspect :ada:`Predicate` is GNAT name for:

  - :ada:`Static_Predicate` if predicate is static
  - :ada:`Dynamic_Predicate` otherwise

--------------------------
Useful Static Predicates
--------------------------

* Scalar ranges with **holes**

  .. code:: ada

     type Count is new Natural
       with Static_Predicate => Count /= 10;

     subtype Normal_Float is Float with
       with Static_Predicate =>
         Normal_Float <= -2.0**(-126) or
         Normal_Float = 0.0 or
         Normal_Float >= 2.0**(-126);

* Enumeration of scalar values

  .. code:: ada

     type Serial_Baud_Rate is range 110 .. 1200
       with Static_Predicate =>
         Serial_Baud_Rate in 110 | 300 | 600 | 1200;

* Enumeration ranges with holes

  .. code:: ada

     subtype Weekend is Day
       with Static_Predicate => Day in Saturday | Sunday;

---------------------------------
Useful Dynamic Predicates (1/2)
---------------------------------

* Array types with **fixed lower bound**

  .. code:: ada

     type Message is new String
       with Dynamic_Predicate => Message'First = 1;

  - Also possible with GNAT extension

    .. code:: ada

       type Message is new String(1 .. <>);

* Record with capacity discriminant and size component

  .. code:: Ada

     type Bounded_String (Capacity : Positive) is record
        Value  : String (1 .. Capacity);
        Length : Natural := 0;
     end record
       with Dynamic_Predicate => Length in 0 .. Capacity;

---------------------------------
Useful Dynamic Predicates (1/2)
---------------------------------

* Array type with ordered content

  .. code:: ada

     type Table is array (Index) of Integer
       with Dynamic_Predicate =>
         (for all K in Table'Range =>
           (K = Table'First or else Table(K-1) <= Table(K)));

* Record type with relationship **between** components

  .. code:: ada

     type Bundle is record
        X, Y : Integer;
        CRC  : Unsigned_32;
     end record
       with Dynamic_Predicate => CRC = Math.CRC32 (X, Y);

* Scalar type with arbitrary property

  .. code:: ada

     type Prime is new Positive
       with Dynamic_Predicate =>
         (for all Divisor in 2 .. Prime / 2 =>
           Prime mod Divisor /= 0);

-----------------------
Restrictions in Usage
-----------------------

* Type with predicate :ada:`T` not allowed for some usages

  - As an array index

    .. code:: ada

       type Table is array (T) of Integer; -- Illegal

  - As a slice

    .. code:: ada

       Var := Param(T); -- Illegal

  - As prefix of attributes :ada:`Range`, :ada:`First`, and :ada:`Last`

    + Because they reflect only range constraints, not predicates
    + Use instead attributes :ada:`First_Valid` and :ada:`Last_Valid`
    + Not allowed on type with dynamic predicate

|

* Type with dynamic predicate further restricted

  - Not allowed as range in a *for loop*
  - Not allowed as choice in *case statement* or *case expression*

--------------------------------
Dynamic Checking of Predicates
--------------------------------

* Dynamic checks inserted by GNAT

  - When using switch :command:`-gnata`
  - Or pragma :ada:`Assertion_Policy (Predicate => Check)`

|

* Placement of checks **similar** as for type constraints

  - On assignment and initialization
  - On conversion :ada:`T(...)` and qualification :ada:`T'(...)`
  - On parameter passing in a call

|

* No checks where not needed

  - On uninitialized objects
  - On references to an object

|

* No checks where that would be too expensive

  - On assigning a part of the object

-------------------------------
Static Checking of Predicates
-------------------------------

* Static checks performed by :toolname:`GNATprove`

  - Always (independent of the choice of switches or pragmas)

|

* Placement of checks as for dynamic checks

  - Plus assignment on part of the object
  - :toolname:`GNATprove` checks objects **always** satisfy their predicate

|

* No checks only where not needed

  - On uninitialized objects
  - On references to an object

|

* :toolname:`GNATprove` can assume that all initialized objects satisfy their
  type constraints and predicates

--------------------------------
Beware Recursion In Predicates
--------------------------------

* Infinite recursion when calling inside the predicate a function taking the
  type with predicate as parameter type

  .. code:: Ada

     type Nat is new Integer
       with Predicate => Above_Zero (Nat);
     function Above_Zero (X : Nat) return Boolean is (X >= 0);

  .. code:: console

     warning: predicate check includes a call to "Above_Zero"
       that requires a predicate check
     warning: this will result in infinite recursion
     warning: use an explicit subtype of "Nat" to carry the predicate
     high: infinite recursion might occur

* Fix by **inlining the property** or introducing a **subtype**

  .. code:: Ada

     type Int is new Integer;
     function Above_Zero (X : Int) return Boolean is (X >= 0);
     subtype Nat is Int with Predicate => Above_Zero (Nat);

=================
Type Invariants
=================

---------------------------
What is a Type Invariant?
---------------------------

* Boolean property that should always hold of objects of the type

  - ...**outside** of its unit
  - Same use of name of the type and component names as in predicates

* Can only be specified on the completion of a private type (in SPARK)

  .. code:: Ada

     package Bank is
       type Account is private;
       type Currency is delta 0.01 digits 12;
       ...
     private
       type Account is ... with
         Type_Invariant => Consistent_Balance (Account);

* **Not** part of membership test :ada:`X in T`

-------------------------------------
Dynamic Checking of Type Invariants
-------------------------------------

* Dynamic checks inserted by GNAT

  - When using switch :command:`-gnata`
  - Or pragma :ada:`Assertion_Policy (Type_Invariant => Check)`

* Placement of checks on the creation of values of type :ada:`T`

  - Note: that applies to objects with a part of type :ada:`T`
  - On default initial value
  - On type conversion :ada:`T(...)`
  - On parameter passing after a call to a :dfn:`boundary subprogram`

    + i.e. call to a subprogram in the public spec of the package

* No checks where not needed

  - On assignment and initialization
  - On qualification :ada:`T'(...)`
  - On references to an object
  - On **internal** assignment or call

* No checks where this is impossible for the compiler

  - On **global** variables of type :ada:`T`
  - On parts of objects under components of **access** type

------------------------------------
Static Checking of Type Invariants
------------------------------------

* Static checks performed by :toolname:`GNATprove`

  - **Always** where needed (independent of the choice of switches or pragmas)

* Placement of checks as for dynamic checks

  - **Plus** global variables and objects under access types
  - On each call to external subprogram from inside the unit

    + This avoids so-called :dfn:`reentrancy problems`

  - :toolname:`GNATprove` checks objects **always** satisfy their invariant
    outside of their unit

* No checks only where not needed
* :toolname:`GNATprove` can assume that all inputs to *boundary subprograms* and all objects of the type outside the unit
  satisfy their type invariants

  - Type invariant is used both for proof of unit itself and in other units
  - An expression function deferred to the body can be used to perform an abstraction

-------------------------------------
Beware Recursion In Type Invariants
-------------------------------------

* Infinite recursion when calling inside the type invariant a *boundary
  function* taking the type with invariant as parameter type

  .. code:: Ada

     package Bank is
        type Account is private;
        function Consistent_Balance (A : Account) return Boolean;
     private
        type Account is ... with
          Type_Invariant => Consistent_Balance (Account);

  .. code:: console

     high: cannot call boundary subprogram for type in its own invariant

* Fix by declaring the function in the **private** part of the spec

  .. code:: Ada

     private
        type Account is ... with
          Type_Invariant => Consistent_Balance (Account);
        function Consistent_Balance (A : Account) return Boolean
          is (...);

=====
Lab
=====

.. include:: labs/9_type_contracts.lab.rst

=========
Summary
=========

----------------
Type Contracts
----------------

* Type contracts given by

  - Type constraints (range and discriminant constraints)
  - Type predicates with aspect :ada:`Predicate`
  - Type invariants with aspect :ada:`Type_Invariant`

* Type predicates are static or dynamic

  - Special aspects :ada:`Static_Predicate` and :ada:`Dynamic_Predicate`
  - Slightly different use cases

* Type invariants define an abstraction on private types

  - Always hold on objects outside their unit
  - Can be violated inside the unit
