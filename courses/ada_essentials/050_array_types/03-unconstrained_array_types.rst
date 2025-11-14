===========================
Unconstrained Array Types
===========================

---------------------------------------
Unconstrained Array Type Declarations
---------------------------------------

* Do not specify bounds for objects
* Thus different objects of the same type may have different bounds
* Bounds cannot change once set

**Syntax**

.. container:: source_include 050_array_types/syntax.bnf :start-after:unconstrained_array_type_declarations_begin :end-before:unconstrained_array_type_declarations_end :code:bnf

**Examples**

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

      Weekdays(Sat) := 0.0; -- Constraint error
      Weekend(Mon)  := 0.0; -- Constraint error

---------------------------------------
Bounds Must Satisfy Type Constraints
---------------------------------------

* Must be somewhere in the range of possible values specified by the type declaration
* :ada:`Constraint_Error` otherwise

.. code:: Ada
   :number-lines: 1

   type Index is range 1 .. 100;
   type Char_Arr is array (Index range <>) of Character;
   Good : Char_Arr (50 .. 75);
   Bad  : Char_Arr (0 .. 10); -- run-time error

.. container:: latex_environment tiny

  :error:`example.adb:5:21: warning: static value out of range of type "Index" defined at line 2`

------------------
Null Index Range
------------------

* When :ada:`'Last` of the range is smaller than :ada:`'First`

  * Array is empty - no components

* When using literals, the compiler will allow out-of-range numbers to indicate empty range

  * Provided values are within the index's base type

  .. code:: Ada
    :number-lines: 2

    type Index_T is range 1 .. 100; -- Index_T'Size = 8

    type Array_T is array (Index_T range <>) of Integer;

    Typical_Empty_Array : Array_T (1 .. 0);
    Weird_Empty_Array   : Array_T (123 .. -5);
    Bad_Empty_Array     : Array_T (999 .. 0);

  .. container:: latex_environment scriptsize

    :error:`example.adb:8:35: error: value not in range of type "Index_T" defined at line 2`

* When the index type is a single-valued enumerated type, no empty array is possible

----------------
Indefinite Types
----------------

* An :dfn:`indefinite type` does not provide enough information to be instantiated

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

* Arrays: consecutive components of the exact **same type**
* Component size must be **defined**

    - No indefinite types
    - No unconstrained types
    - Constrained subtypes allowed

.. container:: latex_environment small

  .. code:: Ada
     :number-lines: 2

     type Component_T is array (Integer range <>) of Boolean;
     type Good is array (1 .. 10) of Component_T (1 .. 20); -- OK
     type Bad is array (1 .. 10) of Component_T; -- compile error

.. container:: latex_environment scriptsize

  :error:`example.adb:4:35: error: unconstrained element type in array declaration`

.. container:: speakernote

   How big is each component for Bad?

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

   A. ``AAA : Bit_Array_T (0..99);``
   B. :answermono:`BBB : Bit_Array_T (1..32);`
   C. :answermono:`CCC : Bit_Array_T (17..16);`
   D. ``DDD : Bit_Array_T;``

 .. container:: column

  .. container:: animate

   Explanations

   A. :ada:`Bit_Array_T` index is :ada:`Positive` which starts at 1
   B. OK, indexes are in range
   C. OK, indicates a zero-length array
   D. Object must be constrained
