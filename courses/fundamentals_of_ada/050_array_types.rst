
*************
Array Types
*************

.. role:: ada(code)
   :language: ada

==============
Introduction
==============

--------------
Introduction
--------------

* Traditional array concept supported to any dimension

.. code:: Ada

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

* Index type

   - Specifies the values to be used to access the array components

* Component type

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

.. code:: Ada

   type Schedule is array (Days range Mon .. Fri) of Real;
   type Bits32_T is array (0 .. 31) of Bit_T;
   type Flags_T is array ( -10 .. 10 ) of Boolean;
   -- this may or may not be null range
   type Dynamic is array (1 .. N) of Integer;

-------------------------
Run-Time Index Checking
-------------------------

* Array indices are checked at run-time as needed
* Invalid index values result in `Constraint_Error`

.. code:: Ada

   procedure Test is
     type List is array (1..10) of Integer;
     A : List;
     K : Integer;
   begin
     A := (others => 0);
     K := FOO;
     A (K) := 42; -- runtime error if Foo returns < 1 or > 10
     Put_Line (A(K)'Img);
   end Test;

----------------------
Kinds of Array Types
----------------------

* Constrained Array Types

   - Bounds specified by type declaration
   - All objects of the type have the same bounds

* Unconstrained Array Types

   - Bounds not specified by type declaration
   - More flexible
   - Allows having objects of the same type but different bounds

   .. code:: Ada

      S1 : String (1 .. 50);
      S2 : String (35 .. 95);
      S3 : String (1 .. 1024);

=========================
Constrained Array Types
=========================

----------
Examples
----------

.. include:: examples/050_array_types/constrained_array_types.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/050_array_types.html#constrained-array-types`

-------------------------------------
Constrained Array Type Declarations
-------------------------------------

* Syntax

      .. code:: Ada

         constrained_array_definition ::=
            array index_constraint of subtype_indication
         index_constraint ::= ( discrete_subtype_definition
            {, discrete_subtype_indication} )
         discrete_subtype_definition ::=
            discrete_subtype_indication | range
         subtype_indication ::= subtype_mark [constraint]
         range ::= range_attribute_reference |
            simple_expression .. simple_expression

* Examples

   .. code:: Ada

      type Full_Week_T is array (Days) of Real;
      type Work_Week_T is array (Days range Mon .. Fri) of Real;
      type Weekdays is array (Mon .. Fri) of Real;
      type Workdays is array (Weekdays'Range) of Real;

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

   type Array1_T is array ( 1 .. 8 ) of boolean;
   type Array2_T is array ( 0 .. 7 ) of boolean;
   X1, Y1 : Array1_T;
   X2, Y2 : Array2_T;

Which statement is not legal?

   A. ``X1(1) := Y1(1);``
   B. ``X1 := Y1;``
   C. ``X1(1) := X2(1);``
   D. :answermono:`X2 := X1;`

.. container:: animate

   Explanations

   A. Legal - elements are :ada:`Boolean`
   B. Legal - object types match
   C. Legal - elements are :ada:`Boolean`
   D. Although the sizes are the same and the elements are the same, the type is different

===========================
Unconstrained Array Types
===========================

----------
Examples
----------

.. include:: examples/050_array_types/unconstrained_array_types.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/050_array_types.html#unconstrained-array-types`

---------------------------------------
Unconstrained Array Type Declarations
---------------------------------------

* Do not specify bounds for objects
* Thus different objects of the same type may have different bounds
* Bounds cannot change once set
* Syntax (with simplifications)

   .. code:: Ada

      unconstrained_array_definition ::=
         array ( index_subtype_definition
            {, index_subtype_definition} )
            of subtype_indication
      index_subtype_definition ::= subtype_mark range <>

* Examples

   .. code:: Ada

      type Index is range 1 .. Integer'Last;
      type CharList is array (Index range <>) of Character;

-----------------------------------------
Supplying Index Constraints for Objects
-----------------------------------------

* Bounds set by:

   - Object declaration
   - Constant's value
   - Variable's initial value
   - Further type definitions (shown later)
   - Actual parameter to subprogram (shown later)

* Once set, bounds never change

   .. code:: Ada

      type Schedule is array (Days range <>) of Real;
      Work : Schedule (Mon .. Fri);
      All_Days : Schedule (Days);

---------------------------------------
Bounds Must Satisfy Type Constraints
---------------------------------------

* Must be somewhere in the range of possible values specified by the type declaration
* `Constraint_Error` otherwise

.. code:: Ada

   type Index is range 1 .. 100;
   type List is array (Index range <>) of Character;
   ...
   Wrong : List (0 .. 10);  -- runtime error
   OK : List (50 .. 75);

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
Specifying Constraints via Initial Value
------------------------------------------

* Lower bound is `Index_subtype'First`
* Upper bound is taken from number of items in value

.. code:: Ada

   subtype Positive is Integer range 1 .. Integer'Last;
   type String is array (Positive range <>)
       of Character;
   ...
   M : String := "Hello World!";
   -- M'first is positive'first (1)

   type Another_String is array (Integer range <>)
       of Character;
   ...
   M : Another_String := "Hello World!";
   -- M'first is integer'first

----------------------------------
No Unconstrained Component Types
----------------------------------

* Arrays: consecutive elements of the exact **same type**
* Component size must be **defined**

    - No unconstrained types
    - Constrained subtypes allowed

.. code:: Ada

   type List is array (1 .. 10) of String (1 .. 20); -- OK
   type List is array (1 .. 10) of String; -- Illegal

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

Which statement is illegal?

   A. :answermono:`X(1) := Y(1);`
   B. ``Y(1) := Z(1);``
   C. ``Y := X;``
   D. ``Z := X;``

.. container:: animate

   Explanations

   A. First index of :ada:`Array_T` is first value in :ada:`Integer` - so :ada:`X(1)` is not in range
   B. Legal - indices are both in range
   C. Legal - arrays are same type and same size
   D. Legal - :ada:`X` has been constrained to correct size

============
Attributes
============

----------
Examples
----------

.. include:: examples/050_array_types/attributes.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/050_array_types.html#attributes`

------------------
Array Attributes
------------------

* Return info about array index bounds

   :T'Length: number of array components
   :T'First: value of lower index bound
   :T'Last: value of upper index bound
   :T'Range: another way of saying `T'First` .. `T'Last`

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

         type List is array (5 .. 15) of Integer;
         L : List;
         List_Index : Integer range List'Range := List'First;
         Count : Integer range  0 .. List'Length := 0;
      begin
         ...
         for K in L'Range loop
            L (K) := K * 2;
         end loop;

.. container:: speakernote

   K will always be a valid index

--------------------------------
Nth Dimension Array Attributes
--------------------------------

.. container:: columns

 .. container:: column

    * Attribute parameter indicates dimension requested

       - ``T'Length(n)``
       - ``T'First(n)``
       - ``T'Last(n)``
       - ``T'Range(n)``
       - where n is the dimension required, including 1

          + if 'n' is not specified, it defaults to 1

 .. container:: column

    .. code:: Ada

       type Two_Dimensioned is array
          (1 .. 10, 12 .. 50) of T;
       TD : Two_Dimensioned;

    * `TD'First` (2) is 12
    * `TD'Last` (2) is 50
    * `TD'Length` (2) is 39
    * `TD'first` is 1 (same as `TD'first(1)`)
    * `TD'last` is 10 (same as `TD'last(1)`)


------
Quiz
------

.. code:: Ada

   subtype Index1_T is Integer range 0 .. 7;
   subtype Index2_T is Integer range 1 .. 8;
   type Array_T is array (0..7, 1..8, Boolean) of Integer;
   X : Array_T;

Which description is incorrect?

   A. ``X'First(2) is 1``
   B. :answermono:`X'Range(3) is True .. False;`
   C. ``X'Length(1) = X'Length(2)``
   D. ``X'Last(1) = 7``

.. container:: animate

   :ada:`Boolean` enumeration is :ada:`( False, True )`

============
Operations
============

----------
Examples
----------

.. include:: examples/050_array_types/operations.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/050_array_types.html#operations`

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

      C := Foo ( B );

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

      type String_Type is array ( integer range <> ) of character;
      A : constant String_Type := "foo";
      B : constant String_Type := "bar";
      C : constant String_Type := A & B;
      -- C now contains "foobar"
     
* Relational (for discrete component types)
* Logical (for Boolean component type)
* Slicing

   - Portion of array

---------
Slicing
---------

* "Slices" out a contiguous part of an array

   - Consisting of zero or more components

* Allowed on any 1-dimensional array type
* Any component type is allowed, not just `Character`

.. code:: Ada

   Full_Name : String := "Monty Python";
   -- print all the characters
   Put_Line (Full_Name);
   -- print the last part
   Put_Line (Full_Name (7..12));
   -- update the first part
   Full_Name (1 .. 5) := "Ariel";

.. list-table::

  * - M

    - O
    - N
    - T
    - Y

    -

    - P
    - Y
    - T
    - H
    - O
    - N

  * - 1

    -
    -
    -

    - 5

    -

    - 7

    -
    -
    -
    -

    - 12

------------------------------
Common Slicing Idiom Example
------------------------------

.. code:: Ada

   function Remove_Spaces (Str : String) return String is
      Ret_Val   : String (1 .. Str'Length);
      Last_Used : Natural := 0;
   begin
      for C in Str'First .. Str'Last loop
         if Str (C) /= ' ' then
            Last_Used := Last_Used + 1;
            -- Build return string
            Ret_Val (Last_Used) := Str (C);
         end if;
      end loop;
      -- Return slices of return string that we need
      -- (If Last_Used = 0, will return an empty string)
      return Ret_Val (1 .. Last_Used);
   end Remove_Spaces;

--------------------
"Membership" Tests
--------------------

* Shorthand for constraint checking

   - Range constraints
   - Index constraints
   - et cetera

* Uses reserved word `in`

.. code:: Ada

   procedure Test is
     type Humanity is array (1 .. N) of Age;
     ...
     People : Humanity := Some_Initial_Value;
     Index : Integer range People'First - 1 ..  People'Last :=
             People'First - 1;
   begin
     ...
     for K in People'Range loop
       -- set Index to K if found
       -- desired value
     end loop;
     if Index in People'Range then -- good

------
Quiz
------

.. code:: Ada

   type Index_T is range 1 .. 10;
   type OneD_T is array (Index_T) of Boolean;
   type ThreeD_T is array (Index_T, Index_T, Index_T) of OneD_T;
   A : ThreeD_T;
   B : OneD_T;

Which statement is illegal?

   A. ``B(1) := A(1,2,3)(1) or A(4,3,2)(1);``
   B. ``B := A(2,3,4) and A(4,3,2);``
   C. :answermono:`A(1,2,3..4) := A(2,3,4..5);`
   D. ``B(3..4) := B(4..5)``

.. container:: animate

   Explanations

   A. All three objects are just boolean values
   B. An element of :ada:`A` is the same type as :ada:`B`
   C. No slicing of multi-dimensional arrays
   D. Slicing allowed on single-dimension arrays

==============================
Operations Added for Ada2012
==============================

----------
Examples
----------

.. include:: examples/050_array_types/operations_added_for_ada2012.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/050_array_types.html#operations-added-for-ada2012`

----------------------------------------
Default Initialization for Array Types
----------------------------------------

.. admonition:: Language Variant

   Ada 2012

* Supports constrained and unconstrained array types
* Supports arrays of any dimensionality

   - No matter how many dimensions, there is only one component type

* Uses aspect `Default_Component_Value`

.. code:: Ada

   type Vector is array (Positive range <>) of Float
      with Default_Component_Value => 0.0;

-------------------------------
Two High-Level For-Loop Kinds
-------------------------------

.. admonition:: Language Variant

   Ada 2012

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

---------------------------
Array/Container For-Loops
---------------------------

.. admonition:: Language Variant

   Ada 2012

* Work in terms of elements within an object
* Syntax hides indexing/iterator controls

   .. code:: Ada

      for name of [reverse] array_or_container_object loop
      ...
      end loop;

* Starts with "first" element unless you reverse it
* Loop parameter name is a constant if iterating over a constant, a variable otherwise

----------------------------------
Array Component For-Loop Example
----------------------------------

.. admonition:: Language Variant

   Ada 2012

* Given an array

   .. code:: Ada

        Primes : constant array (1 .. 5) of Integer :=
           (2, 3, 5, 7, 11);

* Component-based looping would look like

   .. code:: Ada

      for P of Primes loop
         Put_Line (Integer'Image (P));
      end loop;

* While index-based looping would look like

   .. code:: Ada

      for P in Primes'range loop
         Put_Line (Integer'Image (Primes(P)));
      end loop;


----------------------------------------
For-Loops with Multidimensional Arrays
----------------------------------------

.. admonition:: Language Variant

   Ada 2012

.. container:: columns

 .. container:: column

    * Same syntax, regardless of number of dimensions
    * As if a set of nested loops, one per dimension

       - Last dimension is in innermost loop, so changes fastest

    * In low-level format looks like

       - for each row loop
       -   for each column loop
       -     print Identity (row, column)
       -   end loop
       - end loop

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
           Put_Line (Float'Image(C));
         end loop;

------
Quiz
------

.. code:: Ada

   declare
      type Array_T is array (1..3, 1..3) of Integer
         with Default_Component_Value => 1;
      A : Array_T;
   begin
      for I in Index_T range 2 .. 3 loop
         for J in Index_T range 2 .. 3 loop
            A (I, J) := I * 10 + J;
         end loop;
      end loop;
      for I of reverse A loop
         Put (I'Image);
      end loop;
   end;

Which output is correct?

   A. 1 1 1 1 22 23 1 32 33
   B. :answer:`33 32 1 23 22 1 1 1 1`
   C. 0 0 0 0 22 23 0 32 33
   D. 33 32 0 23 22 0 0 0 0

.. container:: animate

   Explanations

   A. This is the result if :ada:`reverse` was not specified
   B. Start with last element (3,3) and work backwards
   C. This might be the result without :ada:`Default_Component_Value.` (Zeroes may not be correct - could be any uninitialized value.)
   D. Result without :ada:`Default_Component_Value` and :ada:`reverse`

============
Aggregates
============

----------
Examples
----------

.. include:: examples/050_array_types/aggregates.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/050_array_types.html#aggregates`

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

   type Schedule is array (Mon .. Fri) of Real;
   Work : Schedule;
   Normal : constant Schedule := (8.0, 8.0, 8.0, 8.0, 8.0);
   ...
   Work := (8.5, 8.5, 8.5, 8.5, 6.0);
   ...
   if Work = Normal then ...
   ...
   if Work = (10.0, 10.0, 10.0, 10.0, 0.0) then -- 4-day week ...

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
* Similar to case statement's `others`
* Can be used to apply defaults too

.. code:: Ada

   type Schedule is array (Days) of Real;
   Work : Schedule;
   Normal : constant Schedule := (8.0, 8.0, 8.0, 8.0, 8.0,
                                  others => 0.0);

-------------------
Nested Aggregates
-------------------

* For multiple dimensions
* For arrays of composite component types

.. code:: Ada

   type Matrix is array (Positive range <>
                         Positive range <>) of Real;
   Mat_4x2 : Matrix (1..4, 1..2) := (1 =>  (2.5, 3.0),
                                     2 =>  (1.5, 0.0),
                                     3 =>  (2.1, 0.0),
                                     4 =>  (9.0, 0.0) );

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
      Move_Number := ( -- rows
                       1 => (1, 2, 3),
                       2 => (4, 5, 6),
                       3 => (7, 8, 9),
                       -- columns
                       4 => (1, 4, 7),
                       5 => (2, 5, 8),
                       6 => (3, 6, 9),
                       -- diagonals
                       7 => (1, 5, 9),
                       8 => (3, 5, 7)  );

----------------------------------
Defaults Within Array Aggregates
----------------------------------

.. admonition:: Language Variant

   Ada 2005

* Specified via the ``box`` notation
* Value for component is thus taken as for stand-alone object declaration

   - So there may or may not be a defined default!

* Can only be used with "named association" form

   - But `others` counts as named form

* Syntax

   .. code:: Ada

      discrete_choice_list => <>

* Example

   .. code:: Ada

      type List is array (1 .. N) of Integer;
      Primes : List := (1 => 2, 2 .. N => <>);

------------------------------
Named Format Aggregate Rules
------------------------------

* Bounds cannot overlap

   - Index values must be specified once and only once

* All bounds must be static

   - Avoids run-time cost to verify coverage of all index values
   - Except for single choice format

.. code:: Ada

   type List is array (Integer range <>) of Real;
   Ages : List (1 .. 10) := (1 .. 3 => X, 4 .. 10 => Y);
   -- illegal: 3 appears twice
   Overlap : List (1 .. 10) := (1 .. 4 => X, 3 .. 10 => Y);
   N, M, K, L : Integer;
   -- illegal: cannot determine if
   -- every index covered at compile time
   Not_Static : List (1 .. 10) := (M .. N => X, K .. L => Y);
   -- This is legal
   Values : List (1 .. N) := (1 .. N => X);

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
   C. ``X := (J => -1, J + 1..A'Last => 1);``
   D. ``X := (1..3 => 100, 3..5 => 200);``

.. container:: animate

   Explanations

   A. Cannot mix positional and named notation
   B. Correct - others not needed but is allowed
   C. Dynamic values must be the only choice. (This could be fixed by making :ada:`J` a constant.)
   D. Overlapping index values (3 appears more than once)

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

          A : array ( 1 .. 3 ) of B;

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

* Any single-dimensioned array of some character type is a string type

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
