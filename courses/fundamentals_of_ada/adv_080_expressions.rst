**************************
Expressions Tips & Tricks
**************************

===============
If-Expressions
===============

------------------------
Boolean If-Expressions
------------------------

* Return a value of either True or False

   - :ada:`(if P then Q)` - assuming `P` and `Q` are `Boolean`
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

   - :ada:`(if P then Q else True)`

* So for convenience and elegance it can be omitted

   .. code:: Ada

      Acceptable : Boolean := (if P1 > 0 then P2 > 0 else True);
      Acceptable : Boolean := (if P1 > 0 then P2 > 0);

* Use :ada:`else` if you need to return False at the end

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
        if Today.Day = End_of_Month(Today.Month) then
      ...

* Using if-expression to call :ada:`Leap (Year)` as needed

   .. code:: Ada

      End_Of_Month : constant array (Months) of Days
        := (Sep | Apr | Jun | Nov => 30,
            Feb => (if Leap (Today.Year)
                    then 29 else 28),
            others => 31);
      begin
        if Today.Day /= End_of_Month(Today.Month) then
      ...


