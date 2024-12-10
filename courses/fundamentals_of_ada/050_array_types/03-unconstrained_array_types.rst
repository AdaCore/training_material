===========================
Unconstrained Array Types
===========================

---------------------------------------
Unconstrained Array Type Declarations
---------------------------------------

* Do not specify bounds for objects
* Thus different objects of the same type may have different bounds
* Bounds cannot change once set
* Syntax (with simplifications)

   .. code:: Ada

      unconstrained_array_definition ::=
         array (index_subtype_definition
            {, index_subtype_definition})
            of subtype_indication
      index_subtype_definition ::= subtype_mark range <>

* Examples

   .. code:: Ada

      type Index is range 1 .. Integer'Last;
      type Char_Arr is array (Index range <>) of Character;

-----------------------------------------
Supplying Index Constraints for Objects
-----------------------------------------

.. code:: Ada

   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Schedule is array (Days range <>) of Float;

* Bounds set by:

   - Object declaration

      .. code:: Ada

         Weekdays : Schedule(Mon..Fri);

   - Object (or constant) initialization

      .. code:: Ada

         Weekend : Schedule := (Sat => 4.0, Sun => 0.0);
         -- (Note this is an array aggregate, explained later)

   - Further type definitions (shown later)
   - Actual parameter to subprogram (shown later)

* Once set, bounds never change

   .. code:: Ada

      Weekdays(Sat) := 0.0; --  Constraint error
      Weekend(Mon)  := 0.0; --  Constraint error

---------------------------------------
Bounds Must Satisfy Type Constraints
---------------------------------------

* Must be somewhere in the range of possible values specified by the type declaration
* :ada:`Constraint_Error` otherwise

.. code:: Ada

   type Index is range 1 .. 100;
   type Char_Arr is array (Index range <>) of Character;
   ...
   Wrong : Char_Arr (0 .. 10);  -- run-time error
   OK : Char_Arr (50 .. 75);

------------------
Null Index Range
------------------

* When :ada:`'Last` of the range is smaller than :ada:`'First`

  * Array is empty - no elements

* When using literals, the compiler will allow out-of-range numbers to indicate empty range

  * Provided values are within the index's base type

  .. code:: Ada

   type Index_T is range 1 .. 100;
   --  Index_T'Size = 8

   type Array_T is array (Index_T range <>) of Integer;

   Typical_Empty_Array : Array_T (1 .. 0);
   Weird_Empty_Array   : Array_T (123 .. -5);
   Illegal_Empty_Array : Array_T (999 .. 0);

* When the index type is a single-valued enumerated type, no empty array is possible

----------------
"String" Types
----------------

* Language-defined unconstrained array types

   - Allow double-quoted literals as well as aggregates
   - Always have a character component type
   - Always one-dimensional

* Language defines various types

   - `String`, with `Character` as component

      .. code:: Ada

         subtype Positive is Integer range 1 .. Integer'Last;
         type String is array (Positive range <>) of Character;

   - `Wide_String`, with `Wide_Character` as component
   - `Wide_Wide_String`, with `Wide_Wide_Character` as component

     - Ada 2005 and later

* Can be defined by applications too

----------------------------------
Application-Defined String Types
----------------------------------

* Like language-defined string types

   - Always have a character component type
   - Always one-dimensional

* Recall character types are enumeration types with at least one character literal value

.. code:: Ada

   type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M');
   type Roman_Number is array (Positive range <>)
       of Roman_Digit;
   Orwellian : constant Roman_Number := "MCMLXXXIV";

------------------------------------------
Specifying Constraints Via Initial Value
------------------------------------------

* Lower bound is :ada:`Index_subtype'First`
* Upper bound is taken from number of items in value

.. code:: Ada

   subtype Positive is Integer range 1 .. Integer'Last;
   type String is array (Positive range <>)
       of Character;
   ...
   M : String := "Hello World!";
   -- M'First is Positive'First (1)

   type Another_String is array (Integer range <>)
       of Character;
   ...
   M : Another_String := "Hello World!";
   -- M'First is Integer'First

----------------
Indefinite Types
----------------

* :dfn:`Indefinite types` do not provide enough information to be instantiated

    - Size
    - Representation

* Unconstrained arrays types are indefinite

    - They do not have a definite :ada:`'Size`

* Other indefinite types exist (seen later)

.. container:: speakernote

   Defined at RM 3-3 (23/5)

-------------------------------
No Indefinite Component Types
-------------------------------

* Arrays: consecutive elements of the exact **same type**
* Component size must be **defined**

    - No indefinite types
    - No unconstrained types
    - Constrained subtypes allowed

.. code:: Ada

   type Good is array (1 .. 10) of String (1 .. 20); -- OK
   type Bad is array (1 .. 10) of String; -- Illegal

.. container:: speakernote

   How big is each component for LIST?

------------------
Arrays of Arrays
------------------

* Allowed (of course!)

   - As long as the "component" array type is constrained

* Indexed using multiple parenthesized values

   - One per array

.. code:: Ada

   declare
      type Array_of_10 is array (1..10) of Integer;
      type Array_of_Array is array (Boolean) of Array_of_10;
      A : Array_of_Array;
   begin
      ...
      A (True)(3) := 42;

------
Quiz
------

.. code:: Ada

   type Bit_T is range 0 .. 1;
   type Bit_Array_T is array (Positive range <>) of Bit_T;

.. container:: columns

 .. container:: column

   Which declaration(s) is (are) legal?

   A. ``AAA : Array_T (0..99);``
   B. :answermono:`BBB : Array_T (1..32);`
   C. :answermono:`CCC : Array_T (17..16);`
   D. ``DDD : Array_T;``

 .. container:: column

  .. container:: animate

   Explanations

   A. :ada:`Array_T` index is :ada:`Positive` which starts at 1
   B. OK, indices are in range
   C. OK, indicates a zero-length array
   D. Object must be constrained
