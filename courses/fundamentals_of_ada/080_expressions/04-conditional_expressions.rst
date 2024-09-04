=========================
Conditional Expressions
=========================

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

..
  language_version 2012

------------------
If Expressions
------------------

* Syntax looks like an *if statement* without :ada:`end if`

   .. code:: Ada

      if_expression ::=
         (if condition then dependent_expression
         {elsif condition then dependent_expression}
         [else dependent_expression])
      condition ::= boolean_expression

   - The conditions are always Boolean values

      .. code:: Ada

         (if Today > Wednesday then 1 else 0)

..
  language_version 2012

-----------------------------------------
Result Must Be Compatible with Context
-----------------------------------------

* The `dependent_expression` parts, specifically

.. code:: Ada

   X : Integer :=
       (if Day_Of_Week (Clock) > Wednesday then 1 else 0);

-------------------------
"If Expression" Example
-------------------------

.. code:: Ada

   declare
     Remaining : Natural := 5;  -- arbitrary
   begin
     while Remaining > 0 loop
       Put_Line ("Warning! Self-destruct in" &
         Remaining'Image &
         (if Remaining = 1 then " second" else " seconds"));
       delay 1.0;
       Remaining := Remaining - 1;
     end loop;
     Put_Line ("Boom! (goodbye Nostromo)");

.. container:: speakernote

   Nostromo - ship from the original Alien :)

--------------------------
Boolean "If Expressions"
--------------------------

* Return a value of either True or False

   - :ada:`(if P then Q)` - assuming `P` and `Q` are `Boolean`
   - "If P is True then the result of the *if expression* is the value of Q"

* But what is the overall result if all conditions are False?
* Answer: the default result value is True

   - Why?

      + Consistency with mathematical proving

.. container:: speakernote

   Mathematical proving: Statements are either True or False.
   If P is false, we don't know anything, so, for mathematical purposes, we assume the statement is true

----------------------------------------
The "else" Part When Result Is Boolean
----------------------------------------

* Redundant because the default result is True

  .. container:: latex_environment  small

   .. code:: Ada

      (if P then Q else True)

* So for convenience and elegance it can be omitted

  .. container:: latex_environment  small

   .. code:: Ada

      Acceptable : Boolean := (if P1 > 0 then P2 > 0 else True);
      Acceptable : Boolean := (if P1 > 0 then P2 > 0);

* Use :ada:`else` if you need to return False at the end

---------------------------------------
Rationale for Parentheses Requirement
---------------------------------------

* Prevents ambiguity regarding any enclosing expression
* Problem:

   .. code:: Ada

      X : Integer := if condition then A else B + 1;

* Does that mean

   - If condition, then `X := A + 1`, else `X := B + 1` **OR**
   - If condition, then `X := A`, else `X := B + 1`

* But not required if parentheses already present

   - Because enclosing construct includes them

      .. code:: Ada

         Subprogram_Call (if A then B else C);

------------------------------
When to Use If Expressions
------------------------------

* When you need computation to be done prior to sequence of statements

   - Allows constants that would otherwise have to be variables

* When an enclosing function would be either heavy or redundant with enclosing context

   - You'd already have written a function if you'd wanted one

* Preconditions and postconditions

   - All the above reasons
   - Puts meaning close to use rather than in package body

* Static named numbers

   - Can be much cleaner than using Boolean'Pos (Condition)

---------------------------------------
"If Expression" Example for Constants
---------------------------------------

* Starting from

   .. code:: Ada

      End_of_Month : array (Months) of Days
        := (Sep | Apr | Jun | Nov => 30,
           Feb => 28,
           others => 31);
      begin
        if Leap (Today.Year) then -- adjust for leap year
          End_of_Month (Feb) := 29;
        end if;
        if Today.Day = End_of_Month (Today.Month) then
      ...

* Using *if expression* to call :ada:`Leap (Year)` as needed

   .. code:: Ada

      End_Of_Month : constant array (Months) of Days
        := (Sep | Apr | Jun | Nov => 30,
            Feb => (if Leap (Today.Year)
                    then 29 else 28),
            others => 31);
      begin
        if Today.Day /= End_of_Month (Today.Month) then
      ...

---------------------
Case Expressions
---------------------

.. container:: latex_environment footnotesize

 * Syntax similar to *case statements*

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

..
  language_version 2012

---------------------------
"Case Expression" Example
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
     End_Of_Month (M) :=
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

Which statement is **not** legal?

   A. :answermono:`F := if X < 0.0 then Sqrt (-1.0 * X) else Sqrt (X);`
   B. ``F := Sqrt (if X < 0.0 then -1.0 * X else X);``
   C. ``B := (if X < 0.0 then Sqrt (-1.0 * X) < 10.0 else True);``
   D. ``B := (if X < 0.0 then Sqrt (-1.0 * X) < 10.0);``

.. container:: animate

   Explanations

   A. Missing parentheses around expression
   B. Legal - Expression is already enclosed in parentheses so you don't need to add more
   C. Legal - :ada:`else True` not needed but is allowed
   D. Legal - :ada:`B` will be :ada:`True` if X >= 0.0

