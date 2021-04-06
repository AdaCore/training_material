*************
Expressions
*************

.. |rightarrow| replace:: :math:`\rightarrow`

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

==================
Membership Tests
==================

----------
Examples
----------

.. include:: examples/080_expressions/membership_tests.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/080_expressions.html#membership-tests`

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
 
=========================
Conditional Expressions
=========================

----------
Examples
----------

.. include:: examples/080_expressions/conditional_expressions.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/080_expressions.html#conditional-expressions`

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

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/080_expressions.html#quantified-expressions`

------------------------
Quantified Expressions
------------------------

.. admonition:: Language Variant

   Ada 2012

* Check if a condition is true on a set

    - Arbitrary boolean **predicate**
    - Any **iterable** set of objects

* "Universal" quantified expressions

   - Predicate is true for :ada:`all` element of the set

* "Existential" quantified expressions

   - Predicate is true for :ada:`some` element of the set
 
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
