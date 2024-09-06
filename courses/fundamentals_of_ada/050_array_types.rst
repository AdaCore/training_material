*************
Array Types
*************

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

--------------
Introduction
--------------

* Traditional array concept supported to any dimension

.. code:: Ada

   declare
      type Hours is digits 6;
      type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      type Schedule is array (Days) of Hours;
      Workdays : Schedule;
   begin
      ...
      Workdays (Mon) := 8.5;

-------------
Terminology
-------------

* :dfn:`Index type`

   - Specifies the values to be used to access the array components

* :dfn:`Component type`

   - Specifies the type of values contained by objects of the array type
   - All components are of this same type

.. code:: Ada

   type Array_T is array (Index_T) of Component_T;

------------------------------
Array Type Index Constraints
------------------------------

* Must be of an integer or enumeration type
* May be dynamic
* Default to predefined `Integer`

   - Same rules as for-loop parameter default type

* Allowed to be null range

   - Defines an empty array
   - Meaningful when bounds are computed at run-time

* Used to define constrained array types

   .. code:: Ada

      type Schedule is array (Days range Mon .. Fri) of Float;
      type Flags_T is array (-10 .. 10) of Boolean;

* Or to constrain unconstrained array types

   .. code:: Ada

      subtype Line is String (1 .. 80);
      subtype Translation is Matrix (1..3, 1..3);

-------------------------
Run-Time Index Checking
-------------------------

* Array indices are checked at run-time as needed
* Invalid index values result in :ada:`Constraint_Error`

.. code:: Ada

   procedure Test is
     type Int_Arr is array (1..10) of Integer;
     A : Int_Arr;
     K : Integer;
   begin
     A := (others => 0);
     K := FOO;
     A (K) := 42; -- run-time error if Foo returns < 1 or > 10
     Put_Line (A(K)'Image);
   end Test;

----------------------
Kinds of Array Types
----------------------

* :dfn:`Constrained` Array Types

   - Bounds specified by type declaration
   - **All** objects of the type have the same bounds

* :dfn:`Unconstrained` Array Types

   - Bounds not constrained by type declaration
   - Objects share the type, but not the bounds
   - More flexible

   .. code:: Ada

      type Unconstrained is array (Positive range <>)
        of Integer;

      U1 : Unconstrained (1 .. 10);
      S1 : String (1 .. 50);
      S2 : String (35 .. 95);

=========================
Constrained Array Types
=========================

-------------------------------------
Constrained Array Type Declarations
-------------------------------------

* Syntax

      .. code:: Ada

         constrained_array_definition ::=
            array index_constraint of subtype_indication
         index_constraint ::= (discrete_subtype_definition
            {, discrete_subtype_indication})
         discrete_subtype_definition ::=
            discrete_subtype_indication | range
         subtype_indication ::= subtype_mark [constraint]
         range ::= range_attribute_reference |
            simple_expression .. simple_expression

* Examples

   .. code:: Ada

      type Full_Week_T is array (Days) of Float;
      type Work_Week_T is array (Days range Mon .. Fri) of Float;
      type Weekdays is array (Mon .. Fri) of Float;
      type Workdays is array (Weekdays'Range) of Float;

----------------------------------
Multiple-Dimensioned Array Types
----------------------------------

.. container:: columns

 .. container:: column

    * Declared with more than one index definition

       - Constrained array types
       - Unconstrained array types

    * Components accessed by giving value for each index

 .. container:: column

   .. container:: latex_environment small

    .. code:: Ada

       type Three_Dimensioned is
         array (
           Boolean,
           12 .. 50,
           Character range 'a' .. 'z')
           of Integer;
         TD : Three_Dimensioned;
         ...
       begin
         TD (True, 42, 'b') := 42;
         TD (Flag, Count, Char) := 42;

-----------------------------
Tic-Tac-Toe Winners Example
-----------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       -- 9 positions on a board
       type Move_Number is range 1 .. 9;
       -- 8 ways to win
       type Winning_Combinations is
          range 1 .. 8;
       -- need 3 positions to win
       type Required_Positions is
          range 1 .. 3;
       Winning : constant array (
          Winning_Combinations,
          Required_Positions)
          of Move_Number := (1 => (1,2,3),
                             2 => (1,4,7),
                             ...

 .. container:: column

    .. list-table::
       :width: 55%

      * - :superscript:`1` **X**

        - :superscript:`2` **X**
        - :superscript:`3` **X**

      * - :superscript:`4`

        - :superscript:`5`
        - :superscript:`6`

      * - :superscript:`7`

        - :superscript:`8`
        - :superscript:`9`

      * -

        -
        -

      * - :superscript:`1` **X**

        - :superscript:`2`
        - :superscript:`3`

      * - :superscript:`4` **X**

        - :superscript:`5`
        - :superscript:`6`

      * - :superscript:`7` **X**

        - :superscript:`8`
        - :superscript:`9`

      * -

        -
        -

      * - :superscript:`1` **X**

        - :superscript:`2`
        - :superscript:`3`

      * - :superscript:`4`

        - :superscript:`5` **X**
        - :superscript:`6`

      * - :superscript:`7`

        - :superscript:`8`
        - :superscript:`9` **X**

------
Quiz
------

.. code:: Ada

   type Array1_T is array (1 .. 8) of Boolean;
   type Array2_T is array (0 .. 7) of Boolean;
   X1, Y1 : Array1_T;
   X2, Y2 : Array2_T;

.. container:: columns

 .. container:: column

   Which statement(s) is (are) legal?

   A. :answermono:`X1 (1) := Y1 (1);`
   B. :answermono:`X1 := Y1;`
   C. :answermono:`X1 (1) := X2 (1);`
   D. ``X2 := X1;``

 .. container:: column

  .. container:: animate

    Explanations

    A. Legal - elements are :ada:`Boolean`
    B. Legal - object types match
    C. Legal - elements are :ada:`Boolean`
    D. Although the sizes are the same and the elements are the same, the type is different

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

   - Further type definitions (shown later)
   - Actual parameter to subprogram (shown later)

* Once set, bounds never change

   .. code:: Ada

      Weekdays(Sat) := 0.0; --  Compiler error
      Weekend(Mon)  := 0.0; --  Compiler error

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

   type Array_T is array (Integer range <>) of Integer;
   subtype Array1_T is Array_T (1 .. 4);
   subtype Array2_T is Array_T (0 .. 3);
   X : Array_T  := (1, 2, 3, 4);
   Y : Array1_T := (1, 2, 3, 4);
   Z : Array2_T := (1, 2, 3, 4);

.. container:: columns

 .. container:: column

   Which statement(s) is (are) legal?

   A. ``X (1) := Y (1);``
   B. :answermono:`Y (1) := Z (1);`
   C. :answermono:`Y := X;`
   D. :answermono:`Z := X;`

 .. container:: column

  .. container:: animate

   Explanations

   A. :ada:`Array_T` starts at :ada:`Integer'First` not :ada:`1`
   B. OK, both in range
   C. OK, same type and size
   D. OK, same type and size

------
Quiz
------

.. code:: Ada

    type My_Array is array (Boolean range <>) of Boolean;

    O : My_Array (False .. False) := (others => True);

What is the value of :ada:`O (True)`?

A. :ada:`False`
B. :ada:`True`
C. None: Compilation error
D. :answer:`None: Run-time error`

.. container:: animate

    :ada:`True` is not a valid index for :ada:`O`.

    NB: GNAT will emit a warning by default.

------
Quiz
------

.. code:: Ada

    type My_Array is array (Positive range <>) of Boolean;

    O : My_Array (0 .. -1) := (others => True);

What is the value of :ada:`O'Length`?

A. 1
B. :answer:`0`
C. None: Compilation error
D. None: Run-time error

.. container:: animate

    When the second index is less than the first index, this is an empty array.
    For empty arrays, the index can be out of range for the index type.

============
Attributes
============

------------------
Array Attributes
------------------

* Return info about array index bounds

   :O'Length: number of array components
   :O'First: value of lower index bound
   :O'Last: value of upper index bound
   :O'Range: another way of saying :ada:`T'First` .. :ada:`T'Last`

* Meaningfully applied to constrained array types

   - Only constrained array types provide index bounds
   - Returns index info specified by the type (hence all such objects)

* Meaningfully applied to array objects

   - Returns index info for the object
   - Especially useful for objects of unconstrained array types

----------------------
Attributes' Benefits
----------------------

* Allow code to be more robust

   - Relationships are explicit
   - Changes are localized

* Optimizer can identify redundant checks

   .. code:: Ada

      declare
         type Int_Arr is array (5 .. 15) of Integer;
         Vector : Int_Arr;
      begin
         ...
         for Idx in Vector'Range loop
            Vector (Idx) := Idx * 2;
         end loop;

  * Compiler understands :ada:`Idx` has to be a valid index for :ada:`Vector`, so no run-time checks are necessary

--------------------------------
Nth Dimension Array Attributes
--------------------------------

* Attribute with **parameter**

.. code:: Ada

  T'Length (n)
  T'First (n)
  T'Last (n)
  T'Range (n)

- ``n`` is the dimension

  + defaults to 1

.. code:: Ada

   type Two_Dimensioned is array
      (1 .. 10, 12 .. 50) of T;
   TD : Two_Dimensioned;

* :ada:`TD'First (2) = 12`
* :ada:`TD'Last  (2) = 50`
* :ada:`TD'Length (2) = 39`
* :ada:`TD'First = TD'First (1) = 1`

------
Quiz
------

.. code:: Ada

   subtype Index1_T is Integer range 0 .. 7;
   subtype Index2_T is Integer range 1 .. 8;
   type Array_T is array (Index1_T, Index2_T) of Integer;
   X : Array_T;

Which comparison is False?

   A. ``X'Last (2) = Index2_T'Last``
   B. :answermono:`X'Last (1)*X'Last (2) = X'Length (1)*X'Length (2)`
   C. ``X'Length (1) = X'Length (2)``
   D. ``X'Last (1) = 7``

.. container:: animate

   Explanations

   A. 8 = 8
   B. 7*8 /= 8*8
   C. 8 = 8
   D. 7 = 7

============
Operations
============

-------------------------
Object-Level Operations
-------------------------

* Assignment of array objects

   .. code:: Ada

      A := B;

* Equality and inequality

   .. code:: Ada

      if A = B then

* Conversions

   .. code:: Ada

      C := Foo (B);

   - Component types must be the same type
   - Index types must be the same or convertible
   - Dimensionality must be the same
   - Bounds must be compatible (not necessarily equal)

-------------------------------
Extra Object-Level Operations
-------------------------------

* *Only for 1-dimensional arrays!*
* Concatenation

   .. code:: Ada

      type String_Type is array
        (Integer range <>) of Character;
      A : constant String_Type := "foo";
      B : constant String_Type := "bar";
      C : constant String_Type := A & B;
      -- C now contains "foobar"

* Comparison (for discrete component types)

   * Not for all scalars

* Logical (for :ada:`Boolean` component type)
* Slicing

   - Portion of array

---------
Slicing
---------

* Contiguous subsection of an array
* On any **one-dimensional** array type

  - Any component type

.. code:: Ada

   procedure Test is
     S1 : String (1 .. 9) := "Hi Adam!!";
     S2 : String := "We love    !";
   begin
     S2 (9..11) := S1 (4..6);
     Put_Line (S2);
   end Test;

Result: ``We love Ada!``

----------------------------------------
Example: Slicing with Explicit Indexes
----------------------------------------

* Imagine a requirement to have a ISO date

  - Year, month, and day with a specific format

.. code:: Ada

   declare
      Iso_Date : String (1 .. 10) := "2024-03-27";
   begin
      Put_Line (Iso_Date);
      Put_Line (Iso_Date (1 .. 4));  --  year
      Put_Line (Iso_Date (6 .. 7));  --  month
      Put_Line (Iso_Date (9 .. 10)); --  day

-----------------------------------
Idiom: Named Subtypes for Indexes
-----------------------------------

* Subtype name indicates the slice index range

   - Names for constraints, in this case index constraints

* Enhances readability and robustness

.. code:: Ada

   procedure Test is
     subtype Iso_Index is Positive range 1 .. 10;
     subtype Year is Iso_Index
        range Iso_Index'First .. Iso_Index'First + 3;
     subtype Month is Iso_Index
        range Year'Last + 2 .. Year'Last + 3;
     subtype Day is Iso_Index
        range Month'Last + 2 .. Month'Last + 3;
     Iso_Date : String (Iso_Index) := "2024-03-27";

   begin
     Put_Line (Iso_Date (Year));  --  2024
     Put_Line (Iso_Date (Month)); --  03
     Put_Line (Iso_Date (Day));   --  27

------------------------------------
Dynamic Subtype Constraint Example
------------------------------------

* Useful when constraints not known at compile-time
* Example: remove file name extension

.. code:: Ada

    File_Name
      (File_Name'First
      ..
      Index (File_Name, '.', Direction => Backward));

------
Quiz
------

.. code:: Ada

   type Index_T is range 1 .. 10;
   type OneD_T is array (Index_T) of Boolean;
   type ThreeD_T is array (Index_T, Index_T, Index_T) of OneD_T;
   A : ThreeD_T;
   B : OneD_T;

Which statement(s) is (are) legal?

   A. :answermono:`B(1) := A(1,2,3)(1) or A(4,3,2)(1);`
   B. :answermono:`B := A(2,3,4) and A(4,3,2);`
   C. ``A(1,2,3..4) := A(2,3,4..5);``
   D. :answermono:`B(3..4) := B(4..5)`

.. container:: animate

   Explanations

   A. All three objects are just Boolean values
   B. An element of :ada:`A` is the same type as :ada:`B`
   C. No slicing of multi-dimensional arrays
   D. Slicing allowed on single-dimension arrays

==============================
Operations Added for Ada2012
==============================

----------------------------------------
Default Initialization for Array Types
----------------------------------------

* Supports constrained and unconstrained array types
* Supports arrays of any dimensionality

   - No matter how many dimensions, there is only one component type

* Uses aspect `Default_Component_Value`

   .. code:: Ada

      type Vector is array (Positive range <>) of Float
         with Default_Component_Value => 0.0;

   - Note that creating a large object of type :ada:`Vector` might incur a run-time cost during initialization

..
  language_version 2012

-------------------------------
Two High-Level For-Loop Kinds
-------------------------------

* For arrays and containers

   - Arrays of any type and form
   - Iterable containers

      + Those that define iteration (most do)
      + Not all containers are iterable (e.g., priority queues)!

* For iterator objects

   - Known as "generalized iterators"
   - Language-defined, e.g., most container data structures

* User-defined iterators too
* We focus on the arrays/containers form for now

..
  language_version 2012

---------------------------
Array/Container For-Loops
---------------------------

* Work in terms of elements within an object
* Syntax hides indexing/iterator controls

   .. code:: Ada

      for name of [reverse] array_or_container_object loop
      ...
      end loop;

* Starts with "first" element unless you reverse it
* Loop parameter name is a constant if iterating over a constant, a variable otherwise

..
  language_version 2012

----------------------------------
Array Component For-Loop Example
----------------------------------

* Given an array

   .. code:: Ada

        type T is array (Positive range <>) of Integer;
        Primes : T := (2, 3, 5, 7, 11);

* Component-based looping would look like

   .. code:: Ada

      for P of Primes loop
         Put_Line (Integer'Image (P));
      end loop;

* While index-based looping would look like

   .. code:: Ada

      for P in Primes'Range loop
         Put_Line (Integer'Image (Primes (P)));
      end loop;

..
  language_version 2012

----------------------------------------
For-Loops with Multidimensional Arrays
----------------------------------------

.. container:: columns

 .. container:: column

    * Same syntax, regardless of number of dimensions
    * As if a set of nested loops, one per dimension

       - Last dimension is in innermost loop, so changes fastest

    * In low-level format looks like

    .. code::

       for each row loop
          for each column loop
             print Identity (
                row, column)
          end loop
       end loop

 .. container:: column

   .. container:: latex_environment small

    .. code:: Ada

       declare
         subtype Rows is Positive;
         subtype Columns is Positive;
         type Matrix is array
            (Rows range <>,
             Columns range <>) of Float;
           Identity : constant Matrix
              (1..3, 1..3) :=
                ((1.0, 0.0, 0.0),
                 (0.0, 1.0, 0.0),
                 (0.0, 0.0, 1.0));
       begin
         for C of Identity loop
           Put_Line (Float'Image (C));
         end loop;

..
  language_version 2012

------
Quiz
------

.. code:: Ada

   declare
      type Array_T is array (1..3, 1..3) of Integer
         with Default_Component_Value => 1;
      A : Array_T;
   begin
      for I in 2 .. 3 loop
         for J in 2 .. 3 loop
            A (I, J) := I * 10 + J;
         end loop;
      end loop;
      for I of reverse A loop
         Put (I'Image);
      end loop;
   end;

.. container:: columns

 .. container:: column

   Which output is correct?

      A. 1 1 1 1 22 23 1 32 33
      B. :answer:`33 32 1 23 22 1 1 1 1`
      C. 0 0 0 0 22 23 0 32 33
      D. 33 32 0 23 22 0 0 0 0

 .. container:: column

  .. container:: animate

     Explanations

     A. There is a :ada:`reverse`
     B. Yes
     C. Default value is 1
     D. No

NB: Without :ada:`Default_Component_Value`, init. values are random

============
Aggregates
============

------------
Aggregates
------------

* Literals for composite types

   - Array types
   - Record types

* Two distinct forms

    - Positional
    - Named

* Syntax (simplified):

   .. code:: Ada

      component_expr ::=
        expression -- Defined value
        | <>       -- Default value

      array_aggregate ::= (
          {component_expr ,}                         -- Positional
        | {discrete_choice_list => component_expr,}) -- Named
        -- Default "others" indices
        [others => expression]

-----------------------------
Aggregate "Positional" Form
-----------------------------

* Specifies array component values explicitly
* Uses implicit ascending index values

.. code:: Ada

   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Working is array (Days) of Boolean;
   Week : Working;
   ...
   -- Saturday and Sunday are False, everything else true
   Week := (True, True, True, True, True, False, False);

------------------------
Aggregate "Named" Form
------------------------

* Explicitly specifies both index and corresponding component values
* Allows any order to be specified
* Ranges and choice lists are allowed (like case choices)

.. code:: Ada

   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Working is array (Days) of Boolean;
   Week : Working;
   ...
   Week := (Sat => False, Sun => False, Mon..Fri => True);
   Week := (Sat | Sun => False, Mon..Fri => True);

--------------------------------------
Combined Aggregate Forms Not Allowed
--------------------------------------

* Some cases lead to ambiguity, therefore never allowed for array types
* Are only allowed for record types (shown in subsequent section)

.. code:: Ada

   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Working is array (Days) of Boolean;
   Week : Working;
   ...
   Week := (True, True, True, True, True, False, False);
   Week := (Sat => False, Sun => False, Mon..Fri => True);
   Week := (True, True, True, True, True,
            Sat => False, Sun => False); -- invalid
   Week := (Sat | Sun => False, Mon..Fri => True);

------------------------------------
Aggregates Are True Literal Values
------------------------------------

* Used any place a value of the type may be used

.. code:: Ada

   type Schedule is array (Mon .. Fri) of Float;
   Work : Schedule;
   Normal : constant Schedule := (8.0, 8.0, 8.0, 8.0, 8.0);
   ...
   Work := (8.5, 8.5, 8.5, 8.5, 6.0);
   ...
   if Work = Normal then
   ...
   if Work = (10.0, 10.0, 10.0, 10.0, 0.0) then -- 4-day week

-----------------------------
Aggregate Consistency Rules
-----------------------------

* Must always be complete

   - They are literals, after all
   - Each component must be given a value
   - But defaults are possible (more in a moment)

* Must provide only one value per index position

   - Duplicates are detected at compile-time

* Compiler rejects incomplete or inconsistent aggregates

   .. code:: Ada

      Week := (Sat => False,
               Sun => False,
               Mon .. Fri => True,
               Wed => False);

.. container:: speakernote

   Wednesday already covered in Monday .. Friday

-----------
 "Others"
-----------

* Indicates all components not yet assigned a value
* All remaining components get this single value
* Similar to case statement's :ada:`others`
* Can be used to apply defaults too

.. code:: Ada

   type Schedule is array (Days) of Float;
   Work : Schedule;
   Normal : constant Schedule := (8.0, 8.0, 8.0, 8.0, 8.0,
                                  others => 0.0);

-------------------
Nested Aggregates
-------------------

* For multiple dimensions
* For arrays of composite component types

.. code:: Ada

   type Matrix is array (Positive range <>,
                         Positive range <>) of Float;
   Mat_4x2 : Matrix (1..4, 1..2) := (1 =>  (2.5, 3.0),
                                     2 =>  (1.5, 0.0),
                                     3 =>  (2.1, 0.0),
                                     4 =>  (9.0, 0.0));

-----------------------------
Tic-Tac-Toe Winners Example
-----------------------------

.. code:: Ada

   type Move_Number is range 1 .. 9;
   -- 8 ways to win
   type Winning_Combinations is range 1 .. 8;
   -- need 3 places to win
   type Required_Positions   is range 1 .. 3;
   Winning : constant array (Winning_Combinations,
                             Required_Positions) of
      Move_Number := (-- rows
                       1 => (1, 2, 3),
                       2 => (4, 5, 6),
                       3 => (7, 8, 9),
                       -- columns
                       4 => (1, 4, 7),
                       5 => (2, 5, 8),
                       6 => (3, 6, 9),
                       -- diagonals
                       7 => (1, 5, 9),
                       8 => (3, 5, 7) );

----------------------------------
Defaults Within Array Aggregates
----------------------------------

* Specified via the ``box`` notation
* Value for component is thus taken as for stand-alone object declaration

   - So there may or may not be a defined default!

* Can only be used with "named association" form

   - But :ada:`others` counts as named form

* Syntax

   .. code:: Ada

      discrete_choice_list => <>

* Example

   .. code:: Ada

      type Int_Arr is array (1 .. N) of Integer;
      Primes : Int_Arr := (1 => 2, 2 .. N => <>);

..
  language_version 2005

------------------------------
Named Format Aggregate Rules
------------------------------

* Bounds cannot overlap

   - Index values must be specified once and only once

* All bounds must be static

   - Avoids run-time cost to verify coverage of all index values
   - Except for single choice format

.. code:: Ada

   type Float_Arr is array (Integer range <>) of Float;
   Ages : Float_Arr (1 .. 10) := (1 .. 3 => X, 4 .. 10 => Y);
   -- illegal: 3 and 4 appear twice
   Overlap : Float_Arr (1 .. 10) := (1 .. 4 => X, 3 .. 10 => Y);
   N, M, K, L : Integer;
   -- illegal: cannot determine if
   -- every index covered at compile time
   Not_Static : Float_Arr (1 .. 10) := (M .. N => X, K .. L => Y);
   -- This is legal
   Values : Float_Arr (1 .. N) := (1 .. N => X);

------
Quiz
------

.. code:: Ada

   type Array_T is array (1 .. 5) of Integer;
   X : Array_T;
   J : Integer := X'First;

Which statement is correct?

   A. ``X := (1, 2, 3, 4 => 4, 5 => 5);``
   B. :answermono:`X := (1..3 => 100, 4..5 => -100, others => -1);`
   C. ``X := (J => -1, J + 1..X'Last => 1);``
   D. ``X := (1..3 => 100, 3..5 => 200);``

.. container:: animate

   Explanations

   A. Cannot mix positional and named notation
   B. Correct - others not needed but is allowed
   C. Dynamic values must be the only choice. (This could be fixed by making :ada:`J` a constant.)
   D. Overlapping index values (3 appears more than once)

------------------------
Aggregates in Ada 2022
------------------------

.. admonition:: Language Variant

   Ada 2022

* Ada 2022 allows us to use square brackets **"[...]"** in defining aggregates

   .. code:: Ada

      type Array_T is array (positive range <>) of Integer;

   * So common aggregates can use either square brackets or parentheses

      .. code:: Ada

         Ada2012 : Array_T := (1, 2, 3);
         Ada2022 : Array_T := [1, 2, 3];

* But square brackets help in more problematic situations

   * Empty array

      .. code:: Ada

         Ada2012 : Array_T := (1..0 => 0);
         Illegal : Array_T := ();
         Ada2022 : Array_T := [];

   * Single element array

      .. code:: Ada

         Ada2012 : Array_T := (1 => 5);
         Illegal : Array_T := (5);
         Ada2022 : Array_T := [5];

--------------------------------
Iterated Component Association
--------------------------------

.. admonition:: Language Variant

   Ada 2022

* With Ada 2022, we can create aggregates with :dfn:`iterators`

   * Basically, an inline looping mechanism

* Index-based iterator

   .. code:: Ada

      type Array_T is array (positive range <>) of Integer;
      Object1 : Array_T(1..5) := (for J in 1 .. 5 => J * 2);
      Object2 : Array_T(1..5) := (for J in 2 .. 3 => J,
                                  5 => -1,
                                  others => 0);

   * :ada:`Object1` will get initialized to the squares of 1 to 5
   * :ada:`Object2` will give the equivalent of :ada:`(0, 2, 3, 0, -1)`

* Component-based iterator

   .. code:: Ada

      Object2 := [for Item of Object => Item * 2];

   * :ada:`Object2` will have each element doubled

-------------------------------
More Information on Iterators
-------------------------------

.. admonition:: Language Variant

   Ada 2022

* You can nest iterators for multiple-dimensioned arrays

   .. code:: Ada

      Matrix : array (1 .. 3, 1 .. 3) of Positive :=
         [for J in 1 .. 3 =>
            [for K in 1 .. 3 => J * 10 + K]];

* You can even use multiple iterators for a single dimension array

   .. code:: Ada

      Ada2012 : Array_T(1..5) := 
         [for I in 1 .. 2 => -1,
          for J in 4 ..5 => 1,
          others => 0];

* Restrictions

   * You cannot mix index-based iterators and component-based iterators in the same aggregate

   * You still cannot have overlaps or missing values

------------------
Delta Aggregates
------------------

.. admonition:: Language Variant

   Ada 2022

.. code:: Ada

   type Coordinate_T is array (1 .. 3) of Float;
   Location : constant Coordinate_T := (1.0, 2.0, 3.0);

* Sometimes you want to copy an array with minor modifications

   * Prior to Ada 2022, it would require two steps

      .. code:: Ada

         declare
            New_Location : Coordinate_T := Location;
         begin
            New_Location(3) := 0.0;
            -- OR
            New_Location := (3 => 0.0, others => <>);
         end;

* Ada 2022 introduces a :dfn:`delta aggregate`

   * Aggregate indicates an object plus the values changed - the *delta*

      .. code:: Ada

         New_Location : Coordinate_T := [Location with delta 3 => 0.0];

* Notes

   * You can use square brackets or parentheses
   * Only allowed for single dimension arrays

*This works for records as well (see that chapter)*

===================================
Detour - 'Image for Complex Types
===================================

------------------
'Image Attribute
------------------

.. admonition:: Language Variant

   Ada 2022

* Previously, we saw the string attribute :ada:`'Image` is provided for scalar types

  * e.g. :ada:`Integer'Image(10+2)` produces the string **" 12"**

* Starting with Ada 2022, the :ada:`Image` attribute can be used for any type

  .. code:: Ada

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Main is
       type Colors_T is (Red, Yellow, Green);
       type Array_T is array (Colors_T) of Boolean;
       Object : Array_T :=
         (Green  => False,
          Yellow => True,
          Red    => True);
    begin
       Put_Line (Object'Image);
    end Main;

  Yields an output of 

  :command:`[TRUE, TRUE, FALSE]`

---------------------------------
Overriding the 'Image Attribute
---------------------------------

.. admonition:: Language Variant

   Ada 2022

* But we don't always want to rely on the compiler defining how we print a complex object

* So we now have the ability to define the :ada:`'Image` functionality by attaching a procedure to the :ada:`Put_Image` aspect

.. code:: Ada

   type Colors_T is (Red, Yellow, Green);
   type Array_T is array (Colors_T) of Boolean with
     Put_Image => Array_T_Image;

-------------------------------
Defining the 'Image Attribute
-------------------------------

.. admonition:: Language Variant

   Ada 2022

* Then we need to declare the procedure

   .. code:: Ada

      procedure Array_T_Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Value  :        Array_T);

   * Which uses the :ada:`Ada.Strings.Text_Buffers.Root_Buffer_Type` as an output buffer
   * (No need to go into detail here other than knowing you do :ada:`Output.Put` to add to the buffer)

* And then we define it

   .. code:: Ada

      procedure Array_T_Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Value  :        Array_T) is
      begin
         for Color in Value'Range loop
            Output.Put (Color'Image & "=>" & Value (Color)'Image & ASCII.LF);
         end loop;
      end Array_T_Image;

----------------------------
Using the 'Image Attribute
----------------------------

.. admonition:: Language Variant

   Ada 2022

* Now, when we call :ada:`Image` we get our "pretty-print" version

  .. code:: Ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Types; use Types;
    procedure Main is
       Object : Array_T := (Green  => False,
                            Yellow => True,
                            Red    => True);
    begin
       Put_Line (Object'Image);
    end Main;

  * Generating the following output

    :command:`RED=>TRUE`

    :command:`YELLOW=>TRUE`

    :command:`GREEN=>FALSE`

* Note this redefinition can be used on any type, even the scalars that have always had the attribute

======================
Anonymous Array Types
======================

-----------------------
Anonymous Array Types
-----------------------

.. container:: columns

 .. container:: column

    * Array objects need not be of a named type

       .. code:: Ada

          A : array (1 .. 3) of B;

    * Without a type name, no object-level operations

       - Cannot be checked for type compatibility
       - Operations on components are still ok if compatible

 .. container:: column

    .. code:: Ada

       declare
       -- These are not same type!
         A, B : array (Foo) of Bar;
       begin
         A := B;  -- illegal
         B := A;  -- illegal
         -- legal assignment of values
         A(J) := B(K);
       end;

========
Lab
========

.. include:: labs/050_array_types.lab.rst

=========
Summary
=========

------------------------------
Final Notes on Type `String`
------------------------------

* Any single-dimensioned array of some character type is a :dfn:`string type`

   - Language defines types `String`, `Wide_String`, etc.

* Just another array type: no null termination
* Language-defined support defined in Appendix A

   - `Ada.Strings.*`
   - Fixed-length, bounded-length, and unbounded-length
   - Searches for pattern strings and for characters in program-specified sets
   - Transformation (replacing, inserting, overwriting, and deleting of substrings)
   - Translation (via a character-to-character mapping)

---------
Summary
---------

* Any dimensionality directly supported
* Component types can be any (constrained) type
* Index types can be any discrete type

   - Integer types
   - Enumeration types

* Constrained array types specify bounds for all objects
* Unconstrained array types leave bounds to the objects

   - Thus differently-sized objects of the same type

* Default initialization for large arrays may be expensive!
* Anonymously-typed array objects used in examples for brevity but that doesn't mean you should in real programs
