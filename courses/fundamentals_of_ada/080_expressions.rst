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
   B : Boolean := X in 0 .. 5;
   C : Boolean := X not in 0 .. 5; -- also "not (X in 0..5)"
   D : Boolean := X in 0 | 2 .. 5; -- Ada 2012

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

   declare
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

   - Explicitly constrain
   - Removing ambiguity of overloading
   - Enhancing readability via explicitness

-------------------------------------------
Explicitly Constraining via Qualification
-------------------------------------------

* Asserts value is compatible with subtype

   - Raises exception :ada:`Constraint_Error` if not true
   - Can reduce :ada:`case` choices

.. code:: Ada

   subtype Weekdays is Days range Mon .. Fri;
   This_Day : Days;
   ...
   --  We assume this_Day is a eeek day
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

-------------------------
Conditional Expressions
-------------------------

.. admonition:: Language Variant

   Ada 2012

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

.. admonition:: Language Variant

   Ada 2012

* Syntax looks like an if-statement without :ada:`end if`

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

   declare
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

---------------------
 *Case Expressions*
---------------------

.. admonition:: Language Variant

   Ada 2012

.. container:: latex_environment footnotesize

 * Syntax similar to :ada:`case` statements

    - Lighter: no closing `end case`
    - Commas between choices

 * Same general rules as *if expressions*

    - Parentheses required unless already present
    - Type of "result" must match context

 * Advantage over *if expressions* is completeness checked by compiler
 * Same as with :ada:`case` statements (unless :ada:`others` is used)

.. code:: Ada

    -- compile error if not all days covered
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

   - Predicate is true for :ada:`all` elements of the set

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

=========
Summary
=========

---------
Summary
---------

* Conditional expressions are allowed wherever expressions are allowed, but beware over-use

   - Especially useful when a constant is intended
   - Especially useful when a static expression is required
