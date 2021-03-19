*************
Expressions
*************

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`

.. role:: ada(code)
    :language: Ada

==============
Introduction
==============

----------------------
Advanced Expressions
----------------------

* Different categories of expressions above simple assignment and conditional statements

   - Constraining types to sub-ranges to increase readability and flexibility

      + Allows for simple membership checks of values

   - Embedded conditional assignments

      + Equivalent to C's ``A ? B : C`` and even more elaborate

   - Universal / Existential checks

      + Ability to easily determine if one or all of a set match a condition

==========
Subtypes
==========

----------
Examples
----------

.. include:: examples/080_expressions/subtypes.rst

----------
Subtypes
----------

* Are not a means for introducing new types
* Declare names for constraints on existing types
* Syntax (simplified)

   .. code:: Ada

      subtype_declaration ::=
         subtype defining_identifier is subtype_indication;
      subtype_indication ::= name [constraint]
 
   - `name` must be an existing type or subtype

.. container:: speakernote

   New keyword "subtype"

-----------------
Subtype Example
-----------------

* A range constraint on an enumeration type

   .. code:: Ada

      type Days is (Sun, Mon, Tues, Wed, Thurs, Fri, Sat);
      subtype Weekdays is Days range Mon .. Fri;
      Workday : Weekdays; -- type Days limited to Mon .. Fri
 
* Meaning is identical to an unnamed constraint 

   .. code:: Ada

      Same_As_Workday : Days range Mon .. Fri;
 
----------------------
Kinds of Constraints
----------------------

* Range constraints on discrete types

   .. code:: Ada

      subtype Positive is Integer range 1 .. Integer'Last;
      subtype Natural is Integer range 0 .. Integer'Last;
      subtype Weekdays is Days range Mon .. Fri;
      subtype Symmetric_Distribution is
          Float range -1.0 .. +1.0;
 
* Index constraints on unconstrained array types

   .. code:: Ada

      subtype Line is String (1 .. 80);
      subtype Translation is Matrix (1..3, 1..3);
 
* Other kinds, discussed in later sections/courses

------------------------
Effects of Constraints
------------------------

* Constraints are only on values

   .. code:: Ada

      type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      subtype Weekdays is Days range Mon .. Fri;
      subtype Weekend is Days range Sat .. Sun;
 
* No functionality is removed

   .. code:: Ada

      subtype Positive is Integer range 1 .. Integer'Last; 
      P : Positive;
      X : Integer := P; -- X and P are the same type
 
---------------------------------
Assignment Respects Constraints
---------------------------------

* RHS values must satisfy LHS constraints
* Exception `Constraint_Error` raised otherwise

.. code:: Ada

   Q : Integer  := some_value;
   P : Positive := Q; -- runtime error if Q <= 0
   N : Natural  := Q; -- runtime error if Q < 0
   J : Integer  := P; -- always legal
   K : Integer  := N; -- always legal

---------------------------------------
Actual Parameters Respect Constraints
---------------------------------------

* Must satisfy any constraints of formal parameters
* `Constraint_Error` otherwise

.. code:: Ada

   declare
     Q : Integer := ...
     P : Positive := ...
     procedure Foo (This : Positive);
   begin
     Foo (Q); -- runtime error if Q <= 0
     Foo (P);
 
----------------------------------------
Attributes Reflect the Underlying Type
----------------------------------------

.. code:: Ada

   type Color is
       (White, Red, Yellow, Green, Blue, Brown, Black);
   subtype Rainbow is Color range Red .. Blue;
 
* `T'First` and `T'Last` respect constraints

   - `Rainbow'first` |rightarrow| Red *but* `Color'first` |rightarrow| White
   - `Rainbow'last` |rightarrow| Blue *but* `Color'last` |rightarrow| Black

* Other attributes reflect base type

   - `Color'Succ (Blue)` = Brown = `Rainbow'Succ (Blue)`
   - `Color'Pos (Blue)` = 4 = `Rainbow'Pos (Blue)`
   - `Color'Val (0)` = White = `Rainbow'Val (0)`

* Assignment must still satisfy target constraints

   .. code:: Ada

      Shade : Color range Red .. Blue := Brown; -- runtime error
      Hue : Rainbow := Rainbow'Succ (Blue);     -- runtime error
 
---------------------------
Range Constraint Examples
---------------------------

.. code:: Ada

   subtype Proper_Subset is Positive range 1 .. 10;
   subtype Same_Constraints is Positive
       range 1 .. Integer'Last;
   subtype Letter is Character range 'A' .. 'z';
   subtype Upper_Case is Letter range 'A' .. 'Z';
   subtype Lower_Case is Letter range 'a' .. 'z';
   subtype Null_Range is Integer
       range 1 .. 0;  -- silly when hard-coded...
   -- evaluated when subtype defined, not when object declared
   subtype Dynamic is Integer range Lower .. Upper;
 
-----------------------------
Stand-Alone (Sub)Type Names
-----------------------------

* Denote all the values of the type or subtype

   - Unless explicitly constrained

* Selected examples

   - All references include all values of positive

   .. code:: Ada

      P : Positive;
      for K in Positive loop ...
      subtype Renamed_Positive is Positive;
 
-------------------------------------
Subtypes and Default Initialization
-------------------------------------

.. admonition:: Language Variant

   Ada 2012

* Not allowed: subtypes don't declare new types

   - Defaults are specified on type declarations only

* **Note:** Objects' default values may violate subtype constraints on object declarations

   - `Constraint_Error` if using variables for subtype definition
   - Compiler error for typical case of a static definition

.. code:: Ada

   type Tertiary_Switch is (Off, On, Neither)
      with Default_Value => Neither;
   subtype Toggle_Switch is Tertiary_Switch
       range Off .. On;
   Safe : Toggle_Switch := Off;
   Implicit : Toggle_Switch; -- compile error: out of range

------
Quiz
------

.. code:: Ada

   type Enum_T is (Sat, Sun, Mon, Tue, Wed, Thu, Fri);
   subtype Enum_Sub_T is Enum_T range Mon .. Fri;
   type Array_T is array (Integer range <>) of Integer;
   subtype Array_Sub_T is Array_T (1 .. 100);

Which subtype definition is valid?

   A. ``subtype A is Enum_Sub_T range Enum_Sub_T'Pred (Enum_Sub_T'First) .. Enum_Sub_T'Last;``
   B. ``subtype B is Array_Sub_T (1 .. 10);``
   C. :answermono:`subtype C is String;`
   D. ``subtype D is digits 6;``

.. container:: animate

   Explanations

   A. This generates a run-time error because the first enumeral specified is not in the range of :ada:`Enum_Sub_T`
   B. Compile error - array type is already constrained
   C. Correct - standalone subtype
   D. :ada:`Digits 6` is used for a type definition, not a subtype

==================
Membership Tests
==================

----------
Examples
----------

.. include:: examples/080_expressions/membership_tests.rst

-------------------------
 "Membership" Operation
-------------------------

* Syntax

   .. code:: Ada

      simple_expression [not] in membership_choice_list
      membership_choice_list ::= membership_choice
                                 { | membership_choice}
      membership_choice ::= expression | range | subtype_mark
 
* Acts like a boolean function
* Usable anywhere a boolean value is allowed

.. code:: Ada

   X : Integer := ... 
   B : Boolean := X in 0..5;
   C : Boolean := X not in 0..5; -- also "not (X in 0..5)"
 
------------------------------------
Testing Constraints via Membership
------------------------------------

.. code:: Ada

   type Calendar_Days  is
       (Mon, Tues, Wed, Thur, Fri, Sat, Sun);
   subtype Weekdays is Calendar_Days range Mon .. Fri;
   Day : Calendar_Days := Today;
   ...
   if Day in Mon .. Fri then ...
   if Day in Weekdays then ... - same as above
 
-----------------------------------
Testing Non-Contiguous Membership
-----------------------------------

.. admonition:: Language Variant

   Ada 2012

* Uses vertical bar "choice" syntax

.. code:: Ada

    M : Month_Number := Month (Clock); 
   begin
     if M in 9 | 4 | 6 | 11 then
       Put_Line ("31 days in this month");
     elsif M = 2 then 
       Put_Line ("It's February, who knows?");
     else
       Put_Line ("30 days in this month");
     end if;
 
------
Quiz
------

.. code:: Ada

   type Days_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   subtype Weekdays_T is Days_T range Mon .. Fri;
   Today : Days_T;

Which condition is illegal?

   A. :answermono:`if Today = Mon or Wed or Fri then`
   B. ``if Today in Days_T then``
   C. ``if Today not in Weekdays_T then``
   D. ``if Today in Tue | Thu then``

.. container:: animate

   Explanations

   A. To use :ada:`or`, both sides of the comparison must be duplicated (e.g. :ada:`Today = Mon or Today = Wed`)
   B. Legal - should always return :ada:`True`
   C. Legal - returns :ada:`True` if :ada:`Today` is :ada:`Sat` or :ada:`Sun`
   D. Legal - returns :ada:`True` if :ada:`Today` is :ada:`Tue` or :ada:`Thu`

=================
Qualified Names
=================

---------------
Qualification
---------------

* Explicitly indicates the subtype of the value
* Syntax

   .. code:: Ada

      qualified_expression ::= subtype_mark'(expression) |
                               subtype_mark'aggregate
 
* Similar to conversion syntax

   - Mnemonic - "qualification uses quote"

* Various uses shown in course

   - Testing constraints
   - Removing ambiguity of overloading
   - Enhancing readability via explicitness

---------------------------------------
Testing Constraints via Qualification
---------------------------------------

* Asserts value is compatible with subtype

   - Raises exception `Constraint_Error` if not true

.. code:: Ada

   subtype Weekdays is Days range Mon .. Fri;
   This_Day : Days;
   ...
   case Weekdays'(This_Day) is --runtime error if out of range
     when Mon =>
       Arrive_Late;
       Leave_Early;
     when Tue .. Thur =>
       Arrive_Early;
       Leave_Late;
     when Fri =>
       Arrive_Early;
       Leave_Early;
   end case; -- no 'others' because all subtype values covered
 
-------------------
Index Constraints
-------------------

* Specify bounds for unconstrained array types

   .. code:: Ada

      type Vector is array (Positive range <>) of Real;
      subtype Position_Vector is Vector (1..3);
      V : Position_Vector;
 
* Index constraints must not already be specified

   .. code:: Ada

      type String is array (Positive range <>) of Character;
      subtype Full_Name is String(1 .. Max); 
      subtype First_Name is Full_Name(1 .. N); -- compile error
 
========
Slices
========

----------
Examples
----------

.. include:: examples/080_expressions/slices.rst

---------
Slicing
---------

.. container:: columns

 .. container:: column
  
    * Specifies a contiguous subsection of an array
    * Allowed on any one-dimensional array type

       - Any component type

 .. container:: column
  
    .. code:: Ada
    
       procedure Test is
         S1 : String (1 .. 9)
            := "Hi Adam!!";
         S2 : String
            := "We love    !";
       begin
         Put_Line (S1 (4..6));
         Put_Line (S2);
         S2 (9..11) := S1 (4..6);
         Put_Line (S2);
         S2 (12) := '?';
         Put_Line (S2);
     
-------------------------------
Slicing With Explicit Indexes
-------------------------------

* Imagine a requirement to have a name with two parts: first and last

.. code:: Ada

     Full_Name : String (1 .. 20);
   begin
     Put_Line (Full_Name);
     Put_Line (Full_Name (1..10));  -- first half of name
     Put_Line (Full_Name (11..20)); -- second half of name
 
-----------------------------------------
Slicing With Named Subtypes for Indexes
-----------------------------------------

* Subtype name indicates the slice index range

   - Names for constraints, in this case index constraints

* Enhances readability and robustness

.. code:: Ada

   procedure Test is
     subtype First_Name is Positive range 1 .. 10;
     subtype Last_Name is Positive range 11 .. 20;
     Full_Name : String(First_Name'First..Last_Name'Last);
   begin
     Put_Line(Full_Name(First_Name)); -- Full_Name(1..10)
     if Full_Name (Last_Name) = SomeString then ...
 
------------------------------------
Dynamic Subtype Constraint Example
------------------------------------

* Useful when constraints not known at compile-time
* Example: find the matching "raw" file name

   - If the filename begins with CRW, we set the extension to CRW, otherwise we set it to CR2

.. code:: Ada
    
   -- actual bounds come from initial value
   Image_File_Name : String := ...   
   Matching_Raw_File_Name : String := Image_File_Name;
   subtype Prefix_Part is Positive range Image_File_Name'First ..
                                         Image_File_Name'First + 2;
   subtype Extension_Part is Positive range Image_File_Name'Last - 2 ..
                                            Image_File_Name'Last;
   begin
     if Image_File_Name (Prefix_Part) = "CRW" then
        Matching_Raw_File_Name(Extension_Part) := "CRW";
     else
       Matching_Raw_File_Name(Extension_Part) := "CR2";
 
=========================
Conditional Expressions
=========================

----------
Examples
----------

.. include:: examples/080_expressions/conditional_expressions.rst

-------------------------
Conditional Expressions
-------------------------

* Ultimate value depends on a controlling condition
* Allowed wherever an expression is allowed

   - Assignment RHS, formal parameters, aggregates, etc.

* Similar intent as in other languages

      + Java, C/C++ ternary operation `A ? B : C`
      + Python conditional expressions
      + etc.

* Two forms:

   - *If expressions*
   - *Case expressions*

------------------
*If Expressions*
------------------

* Syntax looks like an if-statement without `end if`

   .. code:: Ada

      if_expression ::=
         (if condition then dependent_expression
         {elsif condition then dependent_expression}
         [else dependent_expression])
      condition ::= boolean_expression
 
   - The conditions are always Boolean values

      .. code:: Ada

         (if Today > Wednesday then 1 else 0)
 
-----------------------------------------
Result Must Be Compatible with Context 
-----------------------------------------

* The `dependent_expression` parts, specifically

.. code:: Ada

   X : Integer :=
       (if Day_Of_Week (Clock) > Wednesday then 1 else 0);
 
-------------------------
*If Expression* Example
-------------------------

.. code:: Ada

     Remaining : Natural := 5;  -- arbitrary
   begin
     while Remaining > 0 loop
       Put_Line ("Warning! Self-destruct in" & 
         Remaining'Img & 
         (if Remaining = 1 then " second" else " seconds"));
       delay 1.0;
       Remaining := Remaining - 1;
     end loop;
     Put_Line ("Boom! (goodbye Nostromo)");
 
.. container:: speakernote

   Nostromo - ship from the original Alien :)

------------------------
Boolean If-Expressions
------------------------

* Return a value of either True or False

   - `(if P then Q)` - assuming `P` and `Q` are `Boolean`
   - "If P is True then the result of the if-expression is the value of Q"

* But what is the overall result if all conditions are False?
* Answer: the default result value is True

   - Why?

      + Consistency with mathematical proving

.. container:: speakernote

   Mathematical proving: Statements are either True or False.
   If P is false, we don't know anything, so, for mathematical purposes, we assume the statement is true

----------------------------------------
The `else` Part When Result Is Boolean
----------------------------------------

* Redundant because the default result is True

   - `(if P then Q else True)`

* So for convenience and elegance it can be omitted

   .. code:: Ada

      Acceptable : Boolean := (if P1 > 0 then P2 > 0 else True);
      Acceptable : Boolean := (if P1 > 0 then P2 > 0);
 
* Use `else` if you need to return False at the end

---------------------------------------
Rationale for Parentheses Requirement
---------------------------------------

* Prevents ambiguity regarding any enclosing expression
* Problem:

   .. code:: Ada

      X : integer := if condition then A else B + 1;
 
* Does that mean

   - If condition, then `X := A + 1`, else `X := B + 1` **OR**
   - If condition, then `X := A`, else `X := B + 1`

* But not required if parentheses already present

   - Because enclosing construct includes them

      .. code:: Ada

         Subprogram_Call(if A then B else C);
 
------------------------------
When To Use *If Expressions*
------------------------------

* When you need computation to be done prior to sequence of statements

   - Allows constants that would otherwise have to be variables

* When an enclosing function would be either heavy or redundant with enclosing context

   - You'd already have written a function if you'd wanted one

* Preconditions and postconditions 

   - All the above reasons
   - Puts meaning close to use rather than in package body

* Static named numbers

   - Can be much cleaner than using Boolean'Pos(condition)

---------------------------------------
*If Expression* Example for Constants
---------------------------------------

* Harder to read
    
   .. code:: Ada
    
      Leap : constant Boolean :=
         (Today.Year mod 4 = 0 and Today.Year mod 100 /= 0) 
         or else (Today.Year mod 400 = 0);
      End_of_Month : array (Months) of Days := (Sep | Apr | Jun | Nov => 30,
                                                Feb => 28,
                                                others => 31);
      begin
        if Leap then -- adjust for leap year
          End_of_Month (Feb) := 29;
        end if;
        if Today.Day = End_of_Month(Today.Month) then
           ...
     
* Becomes easier to read
    
   .. code:: Ada
    
      Leap : constant Boolean :=
         (Today.Year mod 4 = 0 and Today.Year mod 100 /= 0)
         or else (Today.Year mod 400 = 0);
      End_Of_Month : constant array (Months) 
         of Days := (Sep | Apr | Jun | Nov => 30,
                     Feb => (if Leap then 29 else 28), 
                     others => 31);
      begin
        if Today.Day /= End_of_Month(Today.Month) then
           ...

------------------------------
Static Named Numbers Example
------------------------------

* Hard to read

   .. code:: Ada

      Byte_MSB     : constant := Boolean'Pos (
                     Default_Bit_Order = Low_Order_First) * 7; 
      Halfword_MSB : constant := Boolean'Pos (
                     Default_Bit_Order = Low_Order_First) * 15;      
      Word_MSB     : constant := Boolean'Pos (
                     Default_Bit_Order = Low_Order_First) * 31;   
      NextBit      : constant := 1 - (2 * Boolean'Pos (
                     Default_Bit_Order = Low_Order_First));
 
* Becomes easier to read

   .. code:: Ada

      Byte_MSB     : constant :=
                     (if Default_Bit_Order = Low_Order_First then 7 else 0);
      Halfword_MSB : constant :=
                     (if Default_Bit_Order = Low_Order_First then 15 else 0);
      Word_MSB     : constant :=
                     (if Default_Bit_Order = Low_Order_First then 31 else 0);   
      NextBit      : constant :=
                     (if Default_Bit_Order = Low_Order_First then -1 else 1);

---------------------
 *Case Expressions*
---------------------

.. container:: columns

 .. container:: column

   .. container:: latex_environment footnotesize
  
    * Syntax similar to `case` statements

       - Lighter: no closing `end case`
       - Commas between choices

    * Same general rules as *if expressions*

       - Parentheses required unless already present
       - Type of "result" must match context

    * Advantage over *if expressions* is completeness checked by compiler
    * Same as with `case` statements (unless `others` is used)

 .. container:: column
    
    .. code:: Ada
    
       -- compile error if all 
       -- days not covered
       Hours : constant Integer :=
          (case Day_of_Week is
           when Mon .. Thurs => 9,
           when Fri          => 4,
           when Sat | Sun    => 0);

---------------------------
*Case Expression* Example
---------------------------

.. code:: Ada

   Leap : constant Boolean :=
      (Today.Year mod 4 = 0 and Today.Year mod 100 /= 0)
      or else
      (Today.Year mod 400 = 0);
   End_Of_Month : array (Months) of Days;
   ...
   -- initialize array
   for M in Months loop
     End_Of_Month (M):=
        (case M is
         when Sep | Apr | Jun | Nov => 30,
         when Feb => (if Leap then 29 else 28),
         when others => 31);
   end loop;
 
------
Quiz
------

.. code:: Ada

   function Sqrt (X : Float) return Float;
   F : Float;
   B : Boolean;

Which statement is illegal?

   A. :answermono:`F := if X < 0.0 then Sqrt (-1.0 * X) else Sqrt (X);`
   B. ``F := Sqrt( if X < 0.0 then -1.0 * X else X );``
   C. ``B := (if X < 0.0 then Sqrt (-1.0 * X) < 10.0 else True);``
   D. ``B := (if X < 0.0 then Sqrt (-1.0 * X) < 10.0);``

.. container:: animate

   Explanations

   A. Missing parentheses around expression
   B. Legal - Expression is already enclosed in parentheses so you don't need to add more
   C. Legal - :ada:`else True` not needed but is allowed
   D. Legal - :ada:`B` will be :ada:`True` if X >= 0.0

========================
Quantified Expressions
========================

----------
Examples
----------

.. include:: examples/080_expressions/quantified_expressions.rst

------------------------
Quantified Expressions
------------------------

.. admonition:: Language Variant

   Ada 2012

* Expressions that have a Boolean value
* The value indicates something about a set of objects

   - In particular, whether something is True about that set

* That "something" is expressed as an arbitrary boolean expression

   - A so-called "predicate"

* "Universal" quantified expressions

   - Indicate whether predicate holds for all components

* "Existential" quantified expressions

   - Indicate whether predicate holds for at least one component

-----------------------------------------
Semantics Are As If You Wrote This Code
-----------------------------------------

.. admonition:: Language Variant

   Ada 2012

.. code:: Ada

   function Universal (Set : Components) return Boolean is
   begin
     for C of Set loop
       if not Predicate (C) then
         return False;  -- Predicate must be true for all
       end if;
     end loop;
     return True;
   end Universal;
   
   function Existential (Set : Components) return Boolean is
   begin
     for C of Set loop
       if Predicate (C) then
         return True;  -- Predicate need only be true for one
       end if;
     end loop;
     return False;
   end Existential;
 
-------------------------------
Quantified Expressions Syntax
-------------------------------

.. admonition:: Language Variant

   Ada 2012

.. code:: Ada

   quantified_expression ::=
      (for quantifier loop_parameter_specification
             => predicate) |
      (for quantifier iterator_specification =>
             predicate)
   predicate ::= boolean_expression
   quantifier ::= all | some
 
-----------------
Simple Examples
-----------------

.. admonition:: Language Variant

   Ada 2012

.. code:: Ada

   Values : constant array (1 .. 10) of Integer := (...);  
   Is_Any_Even : constant Boolean :=
      (for some V of Values => V mod 2 = 0);
   Are_All_Even : constant Boolean :=
      (for all V of Values => V mod 2 = 0);
 
----------------------
Universal Quantifier
----------------------

.. admonition:: Language Variant

   Ada 2012

* In logic, denoted by |forall| (inverted 'A', for "all")
* "There is no member of the set for which the predicate does not hold"

   - If predicate is False for any member, the whole is False

* Functional equivalent

   .. code:: Ada

      function Universal (Set : Components) return Boolean is
      begin
        for C of Set loop
          if not Predicate (C) then
             return False; -- Predicate must be true for all
          end if;
        end loop;
        return True;
      end Universal;
 
-----------------------------------
Universal Quantifier Illustration
-----------------------------------

.. admonition:: Language Variant

   Ada 2012

* "There is no member of the set for which the predicate does not hold"
* Given a set of integer answers to a quiz, there are no answers that are not 42 (i.e., all are 42)

.. code:: Ada

   Ultimate_Answer : constant := 42; -- to everything...
   Answers : constant array (1 .. 10)
       of Integer := ( ... );
   All_Correct_1 : constant Boolean :=
      (for all Component of Answers =>
         Component = Ultimate_Answer);
   All_Correct_2 : constant Boolean :=
      (for all K in Answers'range =>
         Answers(K) = Ultimate_Answer);
 
-----------------------------------------
Universal Quantifier Real-World Example
-----------------------------------------

.. admonition:: Language Variant

   Ada 2012

.. code:: Ada

   type DMA_Status_Flag is ( ... );
   function Status_Indicated (
     Flag : DMA_Status_Flag)
     return Boolean;
   None_Set : constant Boolean := (
     for all Flag in DMA_Status_Flag =>
       not Status_Indicated (Flag));
 
------------------------
Existential Quantifier
------------------------

.. admonition:: Language Variant

   Ada 2012

* In logic, denoted by |exists| (rotated 'E', for "exists")
* "There is at least one member of the set for which the predicate holds"

   - If predicate is True for any member, the whole is True

* Functional equivalent

   .. code:: Ada

      function Existential (Set : Components) return Boolean is
      begin
        for C of Set loop
          if Predicate (C) then
            return True; -- Need only be true for at least one
          end if;
        end loop;
        return False;
      end Existential;
 
-------------------------------------
Existential Quantifier Illustration
-------------------------------------

.. admonition:: Language Variant

   Ada 2012

* "There is at least one member of the set for which the predicate holds"
* Given set of integer answers to a quiz, there is at least one answer that is 42

.. code:: Ada

   Ultimate_Answer : constant := 42; -- to everything...
   Answers : constant array (1 .. 10)
       of Integer := ( ... );
   Any_Correct_1 : constant Boolean :=
      (for some Component of Answers =>
         Component = Ultimate_Answer);
   Any_Correct_2 : constant Boolean :=
      (for some K in Answers'range =>
         Answers(K) = Ultimate_Answer);
 
-----------------------------------------
Index-Based vs Component-Based Indexing
-----------------------------------------

.. admonition:: Language Variant

   Ada 2012

* Given an array of integers

   .. code:: Ada

      Values : constant array (1 .. 10) of Integer := (...);  

* Component-based indexing is useful for checking individual values

   .. code:: Ada

      Contains_Negative_Number : constant Boolean :=
         (for some N of Values => N < 0);

* Index-based indexing is useful for comparing across values

   .. code:: Ada

      Is_Sorted : constant Boolean :=
         (for all I in Values'Range =>
            I = Values'first or else Values(I) >= Values(I-1));

---------------------------------------
"Pop Quiz" for Quantified Expressions
---------------------------------------

.. admonition:: Language Variant

   Ada 2012

* What will be the value of `Ascending_Order`?

   .. code:: Ada

      Table : constant array (1 .. 10) of Integer :=
            (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
      Ascending_Order : constant Boolean := (
        for all K in Table'Range => 
          K > Table'First and then Table (K - 1) <= Table (K));
 
   - Answer: **False**. Predicate fails when `K = Table'First`

      + First subcondition is False!
      + Condition should be

         .. code:: Ada

          Ascending_Order : constant Boolean := (
             for all K in Table'Range => K = Table'first or else
                                         Table (K - 1) <= Table (K));

---------------------------
 When The Set Is Empty...
---------------------------

.. admonition:: Language Variant

   Ada 2012

* Universally quantified expressions are True

   - Definition: there is no member of the set for which the predicate does not hold
   - If the set is empty, there is no such member, so True
   - "All people 12-feet tall will be given free chocolate."

* Existentially quantified expressions are False

   - Definition: there is at least one member of the set for which the predicate holds

* If the set is empty, there is no such member, so False
* Common convention in set theory, arbitrary but settled

-----------------------------------------
Not Just Arrays: Any "Iterable" Objects
-----------------------------------------

.. admonition:: Language Variant

   Ada 2012

* Those that can be iterated over
* Language-defined, such as the containers
* User-defined too

.. code:: Ada

   package Characters is new
      Ada.Containers.Vectors (Positive, Character);
   use Characters;    
   Alphabet  : constant Vector := To_Vector('A',1) & 'B' & 'C';
   Any_Zed   : constant Boolean :=
              (for some C of Alphabet => C = 'Z');
   All_Lower : constant Boolean :=
               (for all C of Alphabet => Is_Lower (C));

-------------------------------------------
Conditional / Quantified Expression Usage
-------------------------------------------

.. admonition:: Language Variant

   Ada 2012

* Use them when a function would be too heavy
* Don't over-use them!

   .. code:: Ada

      if (for some Component of Answers =>
          Component = Ultimate_Answer)
      then
 
* Function names enhance readability

   - So put the quantified expression in a function

      .. code:: Ada

         if At_Least_One_Answered (Answers) then
 
* Even in pre/postconditions, use functions containing quantified expressions for abstraction

------
Quiz
------

.. code:: Ada

   type Array1_T is array (1 .. 3) of Integer;
   type Array2_T is array (1 .. 3) of Array1_T;
   A : Array2_T;

The above describes an array A whose elements are arrays of three elements.
Which expression would one use to determine if at least one of A's elements are sorted?

A. | ``(for some Element of A =>``
   |    ``(for some Index in 2 .. 3 =>``
   |       ``Element (Index) >= Element (Index - 1)));``
B. | ``(for all Element of A =>``
   |    ``(for all Index in 2 .. 3 =>``
   |       ``Element (Index) >= Element (Index - 1)));``
C. | :answermono:`(for some Element of A =>`
   |    :answermono:`(for all Index in 2 .. 3 =>`
   |       :answermono:`Element (Index) >= Element (Index - 1)));`
D. | ``(for all Element of A =>``
   |    ``(for some Index in 2 .. 3 =>``
   |       ``Element (Index) >= Element (Index - 1)));``

.. container:: animate

   Explanations

   A. Will be :ada:`True` if any element has two consecutive increasing values
   B. Will be :ada:`True` if every element is sorted
   C. Correct
   D. Will be :ada:`True` if every element has two consecutive increasing values

========
Lab
========

.. include:: labs/080_expressions.lab.rst

=========
Summary
=========

--------------------------------
Subtypes Localize Dependencies
--------------------------------

* Single points of change
* Relationships captured in code
* Which is more maintainable

   - No subtypes

      .. code:: Ada

         type List is array (1 .. 12) of Some_Type;
         K : Integer range 0 .. 12;
         Values : List;
         ...
         if K in 1 .. 12 then ...
         for J in Integer range 1 .. 12 loop
 
   - Subtypes

      .. code:: Ada

         type Counter is range 0 .. 12;
         subtype Index is Counter range 1 .. Counter'Last;
         type List is array (Index) of Some_Type;
         K : Counter := 0;
         Values : List;
         ...
         if K in Index then ...
         for J in Index loop ...
 
----------------------------------
Subtypes May Enhance Performance
----------------------------------

* Provides compiler with more information
* Redundant checks can more easily be identified

.. code:: Ada

   subtype Index is Integer range 1 .. Max;
   type List is array (Index) of Float;
   K : Index;
   Values : List;
   ...
   K := Some_Value;   -- range checked here
   Values (K) := 0.0; -- so no range check needed here
 
---------
Summary
---------

* Constraints are very beneficial in their own right

   - Robustness  and performance
   - Naming them is even better

* Conditional expressions are allowed wherever expressions are allowed, but beware over-use

   - Especially useful when a constant is intended
   - Especially useful when a static expression is required

* Quantified expressions are general purpose but especially useful with pre/postconditions

   - Consider hiding them behind expressive function names
