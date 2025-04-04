=================
Type Predicates
=================

---------------------------
What Is a Type Predicate?
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
Static Vs Dynamic Predicate
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
Useful Dynamic Predicates (2/2)
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

|

* Special aspect :ada:`Ghost_Predicate` for referring to ghost entities

  - Type cannot be used in membership tests

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
Beware Recursion in Predicates
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

