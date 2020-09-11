
*************
Array Types
*************

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
   type List is array (1 .. 10) of Some_Type;
   -- silly since hard-coded
   type Nullo is array (1 .. 0) of Some_Type;
   -- this may or may not be null range
   type Dynamic is array (1 .. N) of Integer;

-------------------------
Run-Time Index Checking
-------------------------

* Array indexes are checked at run-time as needed
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

--------------------------------
Constrained Array Type Example
--------------------------------

.. code:: Ada

   subtype User_LED is GPIO_Pin;
   Green  : User_LED renames Pin_1;
   Orange : User_LED renames Pin_13;
   Red    : User_LED renames Pin_8;
   Blue   : User_LED renames Pin_15;
   ...
   type Index is mod 4;
   -- The LEDs are not physically laid out "consecutively" in
   -- such a way that we can simply go in pin value order to
   -- get circular rotation. Thus we define this mapping,
   -- using a consecutive index to get the physical LED
   -- blinking order desired.
   type Blinking_Pattern is array (Index) of User_LED;
   Order : constant Blinking_Pattern :=
           (Orange, Red, Blue, Green);

============
Attributes
============

------------------
Array Attributes
------------------

* Return info about array index bounds

   :T'Length: number of array components
   :T'First: value of lower index bound
   :T'Last: value of upper index bound
   :T'Range: another way of saying T'First .. T'Last

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

* Component values are objects
* Objects of unconstrained types must be constrained
* Once set, bounds never change

.. code:: Ada

   subtype Positive is Integer range 1 .. Integer'Last;
   type String is array (Positive range <>) of Character;
   type List is array (1..10) of String; -- illegal

.. container:: speakernote

   How big is each component for LIST?

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
* Relational (for discrete component types)
* Logical (for Boolean component type)
* Slicing

   - Portion of array

* Sliding

   - Re-indexing array

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

   type Moves is range 1 .. 4;
   type Positions is array (Moves range <>) of Position;
   function Next (Current : Position) return Positions is
     Result : Positions (Moves);
     -- upper bound
     Count : Integer range 0 .. Moves'Last := 0;
   begin
     for each position adjacent to Current loop
       if adjacent_position_is_on_the_board_and_not_a_wall
       then
         Count := Count + 1;
         Result (Count) := adjacent position;
       end if;
     end loop;
     return Result (1 .. Count); -- could be empty
   end Next;

---------
Sliding
---------

* Allows bounds to be different
* But must be compatible (1:1 correspondence)

   - `Constraint_Error` otherwise

.. code:: Ada

      type Vector is array (Integer range <>) of Real;
      V : Vector (1 .. 5);
      Z : Vector (6 .. 10);
   begin
      Z := V;  -- five elements each
      Z (6 .. 8) := V (2 .. 4); -- three elements each
   end;

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

============
Aggregates
============

------------
Aggregates
------------

* Literals for composite types

   - Array types
   - Record types

* Syntax (simplified):

   .. code:: Ada

      array_aggregate ::= positional_array_aggregate |
                          named_array_aggregate

      positional_array_aggregate ::=
            (expression, expression {, expression}) |
            (expression {, expression}, others => expression)

      named_array_aggregate ::= (array_component_association
                                {,array_component_association})

      array_component_association ::=
            discrete_choice_list => expression

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

      named_array_aggregate ::= ( array_component_association
         {, array_component_association} )
      array_component_association ::=
         discrete_choice_list => expression
         |discrete_choice_list => <>

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

----------------------
Table Search Example
----------------------

.. code:: Ada

     type Age is range 0 .. 200;
     type Humanity is array (1 .. N) of Age;
     ...
     People : Humanity := Some_Initial_Value;
     Index : Integer range People'First - 1 .. People'Last :=
        People'First - 1; -- currently not a legal index
   begin
     ...
     for K in People'Range loop
       if People (K) = Desired_Value then
         Index := K;
         exit;
       end if;
     end loop;
     if Index >= People'First and Index <= People'Last
     then
        -- found it

=======================
Additional Operations
=======================

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

      for name of [reverse] array_or_container_object_name loop
      ...
      end loop;

* Starts with "first" element unless you reverse it
* Loop parameter name is a constant if iterating over a constant, a variable otherwise

----------------------------------
Array Component For-Loop Example
----------------------------------

.. admonition:: Language Variant

   Ada 2012

.. code:: Ada

     Primes : constant array (1 .. 5) of Integer :=
        (2, 3, 5, 7, 11);
   begin
     ...
     for P of Primes loop
       Put_Line (Integer'Image (P));
     end loop;
     ...
     for P of reverse Primes loop
       Put_Line (Integer'Image (P));
     end loop;
     ...

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

    .. code:: Ada

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

==========
Examples
==========

-----------------------------
Substitution Cipher Example
-----------------------------

.. code:: Ada

     Scrambled : constant array (Character range 'A'..'Z') of
        Character := ('Z', 'M', 'W', ..., 'I', 'S', 'O');
     Char : Character;
   begin
     loop
       Put ("Enter a letter : ");
       Get (Char);
       exit when Char in Scrambled'Range;
       Put_Line ("Must be in range 'A' .. 'Z' !");
     end loop;
     Put_Line ("Substitution letter for " & Char &
               " is " & Scrambled (Char));
   end;

.. container:: speakernote

  Index is "real" character, component is "switched" character

-----------------------
Roman Numbers Example
-----------------------

.. code:: Ada

     -- order of digits is critical
     type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M');
     type Roman_Number is array (Positive range <>)
         of Roman_Digits;
     Roman_Year : constant Roman_Number := "MCMLXXXIV";
     Decimal_Year : Integer := 0;
     Value_of : constant array (Roman_Digits) of Integer :=
         (1, 5, 10, 50, 100, 500, 1000);
   begin
     for K in Roman_Year'Range loop
       if K /= Roman_Year'Last and then
          Roman_Year(K) < Roman_Year(K+1)
       then
         Decimal_Year := Decimal_Year-Value_of(Roman_Year(K));
       else
         Decimal_Year := Decimal_Year+Value_of(Roman_Year(K));
       end if;
     end loop;
     IO.Put_Line ("MCMLXXXIV = " &
                  Integer'Image (Decimal_Year));
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
