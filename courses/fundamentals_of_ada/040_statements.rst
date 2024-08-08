************
Statements
************

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

-----------------
Statement Kinds
-----------------

* Simple

  - :ada:`null`
  - :ada:`A := B` (assignments)
  - :ada:`exit`
  - :ada:`goto`
  - :ada:`delay`
  - :ada:`raise`
  - :ada:`P (A, B)` (procedure calls)
  - :ada:`return`
  - Tasking-related: :ada:`requeue`, entry call :ada:`T.E (A, B)`, :ada:`abort`

* Compound

  - :ada:`if`
  - :ada:`case`
  - :ada:`loop` (and variants)
  - :ada:`declare`
  - Tasking-related: :ada:`accept`, :ada:`select`

*Tasking-related are seen in the tasking chapter*

----------------------------
Procedure Calls (Overview)
----------------------------

* Procedures must be defined before they are called

  .. code:: Ada

     procedure Activate (This : in out Foo;
                         Flag :        Boolean);  

* Procedure calls are statements

  * Traditional call notation

    .. code:: Ada

      Activate (Idle, True);

  * "Distinguished Receiver" notation

    .. code:: Ada

      Idle.Activate (True);

* More details in "Subprograms" section

==================
Block Statements
==================

------------------
Block Statements
------------------

    * Local **scope**
    * Optional declarative part
    * Used for

       - Temporary declarations
       - Declarations as part of statement sequence
       - Local catching of exceptions

    * Syntax

       .. code:: Ada

          [block-name :]
          [declare <declarative part> ]
          begin
             <statements>
          end [block-name];

--------------------------
Block Statements Example
--------------------------

.. code:: Ada

   begin
      Get (V);
      Get (U);
      if U > V then -- swap them
         Swap: declare
            Temp : Integer;
         begin
            Temp := U;
            U := V;
            V := Temp;
         end Swap;
         -- Temp does not exist here
      end if;
      Print (U);
      Print (V);
   end;

=================
Null Statements
=================

-----------------
Null Statements
-----------------

* Explicit no-op statement
* Constructs with required statement
* Explicit statements help compiler

    - Oversights
    - Editing accidents

.. code:: Ada

   case Today is
     when Monday .. Thursday =>
       Work (9.0);
     when Friday =>
       Work (4.0);
     when Saturday .. Sunday =>
       null;
   end case;

=======================
Assignment Statements
=======================

-----------------------
Assignment Statements
-----------------------

* Syntax

   .. code:: Ada

      <variable> := <expression>;

* Value of expression is copied to target variable
* The type of the RHS must be same as the LHS

   - Rejected at compile-time otherwise

.. container:: latex_environment small

  .. code:: Ada

     declare
        type Miles_T is range 0 .. Max_Miles;
        type Km_T is range 0 .. Max_Kilometers

        M : Miles_T := 2; -- universal integer legal for any integer
        K : Km_T := 2; -- universal integer legal for any integer
     begin
        M := K; -- compile error

----------------------------------------
Assignment Statements, Not Expressions
----------------------------------------

* Separate from expressions

   - No Ada equivalent for these:

      .. code:: C++

         int a = b = c = 1;
         while (line = readline(file))
            { ...do something with line... }

* No assignment in conditionals

   - E.g. :ada:`if (a == 1)` compared to :ada:`if (a = 1)`

------------------
Assignable Views
------------------

* A :dfn:`view` controls the way an entity can be treated

   - At different points in the program text

* The named entity must be an assignable variable

   - Thus the view of the target object must allow assignment

* Various un-assignable views

   - Constants
   - Variables of :ada:`limited` types
   - Formal parameters of mode :ada:`in`

.. code:: Ada

   Max : constant Integer := 100;
   ...
   Max := 200; -- illegal

--------------------------------
Aliasing the Assignment Target
--------------------------------

.. admonition:: Language Variant

   Ada 2022

* C allows you to simplify assignments when the target is used in the expression. This avoids duplicating (possibly long) names.

   .. code:: C

      total = total + value;
      // becomes
      total += value;

* Ada 2022 implements this by using the target name symbol **@**

   .. code:: Ada

      Total := Total + Value;
      -- becomes
      Total := @ + Value;

* Benefit

   * Symbol can be used multiple times in expression

      .. code:: Ada

         Value := (if @ > 0 then @ else -(@));

* Limitation

   * Symbol is read-only (so it can't change during evaluation)

      .. code:: Ada

         function Update (X : in out Integer) return Integer;
         function Increment (X: Integer) return Integer;

      .. code:: Ada
         :number-lines: 13

            Value := Update (@);
            Value := Increment (@);

      ``example.adb:13:21: error: actual for "X" must be a variable``

------
Quiz
------

.. container:: latex_environment scriptsize

 .. container:: columns

  .. container:: column

   .. code:: Ada

      type One_T is range 0 .. 100;
      type Two_T is range 0 .. 100;
      A : constant := 100;
      B : constant One_T := 99;
      C : constant Two_T := 98;
      X : One_T := 0;
      Y : Two_T := 0;

  .. container:: column

   Which block is **not** legal?

   A. | ``X := A;``
      | ``Y := A;``
   B. | ``X := B;``
      | ``Y := C;``
   C. | :answermono:`X := One_T(X + C);`
   D. | ``X := One_T(Y);``
      | ``Y := Two_T(X);``

   .. container:: animate

     Explanations

     A. Legal - :ada:`A` is an untyped constant
     B. Legal - :ada:`B, C` are correctly typed
     C. Illegal - No such "+" operator: must convert operand individually
     D. Legal - Correct conversion and types

========================
Conditional Statements
========================

-------------------------
If-then-else Statements
-------------------------

* Control flow using Boolean expressions
* Syntax

   .. code:: Ada

      if <boolean expression> then -- No parentheses
         <statements>;
      [else
         <statements>;]
      end if;

* At least one statement must be supplied

    - :ada:`null` for explicit no-op

--------------------------
If-then-elsif Statements
--------------------------

* Sequential choice with alternatives
* Avoids :ada:`if` nesting
* :ada:`elsif` alternatives, tested in textual order
* :ada:`else` part still optional

.. container:: columns

 .. container:: column

  .. code:: Ada
     :number-lines: 1

     if Valve (N) /= Closed then
       Isolate (Valve (N));
       Failure (Valve (N));
     else
       if System = Off then
         Failure (Valve (N));
       end if;
     end if;

 .. container:: column

  .. code:: Ada
     :number-lines: 1

     if Valve (N) /= Closed then
       Isolate (Valve (N));
       Failure (Valve (N));
     elsif System = Off then
       Failure (Valve (N));
     end if;

.. container:: speakernote

   Spelled that way on purpose, as was done in Python for example (differently, "elif")

-----------------
Case Statements
-----------------

* Exclusionary choice among alternatives
* Syntax

   .. code:: Ada

      case <expression> is
        when <choice> => <statements>;
        { when <choice> => <statements>; }
      end case;

   .. code::

      choice ::= <expression> | <discrete range>
                | others { "|" <other choice> }

------------------------
Simple "case" Statements
------------------------

.. code:: Ada

   type Directions is  (Forward, Backward, Left, Right);
   Direction : Directions;
   ...
   case Direction is
     when Forward =>
       Set_Mode (Forward);
       Move (1);
     when Backward =>
       Set_Mode (Backup);
       Move (-1);
     when Left =>
       Turn (1);
     when Right =>
       Turn (-1);
   end case;

*Note*: No fall-through between cases

----------------------
Case Statement Rules
----------------------

* More constrained than a if-elsif structure
* **All** possible values must be covered

   - Explicitly
   - ... or with :ada:`others` keyword

* Choice values cannot be given more than once (exclusive)

    - Must be known at **compile** time

------------------
 `Others` Choice
------------------

* Choice by default

    - "everything not specified so far"

* Must be in last position

.. code:: Ada

   case Today is   -- work schedule
     when Monday =>
       Go_To (Work, Arrive=>Late, Leave=>Early);
     when Tuesday | Wednesday | Thursday => -- Several choices
       Go_To (Work, Arrive=>Early, Leave=>Late);
     when Friday =>
       Go_To (Work, Arrive=>Early, Leave=>Early);
     when others => -- weekend
       Go_To (Home, Arrive=>Day_Before, Leave=>Day_After);
   end case;

------------------------------------
Case Statements Range Alternatives
------------------------------------

.. code:: Ada

   case Altitude_Ft is
     when 0 .. 9 =>
       Set_Flight_Indicator (Ground);
     when 10 .. 40_000 =>
       Set_Flight_Indicator (In_The_Air);
     when others => -- Large altitude
       Set_Flight_Indicator (Too_High);
   end case;

------------------------------------
Dangers of *Others* Case Alternative
------------------------------------

* Maintenance issue: new value requiring a new alternative?

    - Compiler won't warn: :ada:`others` hides it

.. code:: Ada

   type Agencies_T is (NASA, ESA, RFSA); -- could easily grow
   Bureau : Agencies_T;
   ...
   case Bureau is
     when ESA =>
        Set_Region (Europe);
     when NASA =>
        Set_Region (America);
     when others =>
        Set_Region (Russia); -- New agencies will be Russian!
   end case;

------
Quiz
------

.. code:: Ada

   A : Integer := 100;
   B : Integer := 200;

Which choice needs to be modified to make a valid :ada:`if` block

A. | :answermono:`if A == B and then A != 0 then`
   |    :answermono:`A := Integer'First;`
   |    :answermono:`B := Integer'Last;`
B. | ``elsif A < B then``
   |    ``A := B + 1;``
C. | ``elsif A > B then``
   |    ``B := A - 1;``
D. | ``end if;``

.. container:: animate

   Explanations

   * :ada:`A` uses the C-style equality/inequality operators
   * :ada:`D` is legal because :ada:`else` is not required

------
Quiz
------

.. code:: Ada

   type Enum_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   A : Enum_T;

Which choice needs to be modified to make a valid :ada:`case` block

.. code:: Ada

   case A is

A. | ``when Sun =>``
   |    ``Put_Line ("Day Off");``
B. | ``when Mon | Fri =>``
   |    ``Put_Line ("Short Day");``
C. | ``when Tue .. Thu =>``
   |    ``Put_Line ("Long Day");``
D. | :answermono:`end case;`

.. container:: animate

   Explanations

   * Ada requires all possibilities to be covered
   * Add :ada:`when others` or :ada:`when Sat`

=================
Loop Statements
=================

------------------------
Basic Loops and Syntax
------------------------

* All kind of loops can be expressed

  - Optional iteration controls
  - Optional exit statements

* Syntax

   .. code:: Ada

     [<name> :] [iteration_scheme] loop
           <statements>
      end loop [<name>];

   .. code::

      iteration_scheme ::= while <boolean expression>
                           | for <loop_parameter_specification>
                           | for <loop_iterator_specification>

* Example

   .. code:: Ada

      Wash_Hair : loop
        Lather (Hair);
        Rinse (Hair);
      end loop Wash_Hair;

.. container:: speakernote

    Loop Iterator Specification available in Ada2012 and later

--------------------
Loop Exit Statements
--------------------

* Leaves innermost loop

   - Unless loop name is specified

* Syntax

   .. code:: Ada

      exit [<loop name>] [when <boolean expression>];

* :ada:`exit when` exits with condition

.. code:: Ada

    loop
      ...
      -- If it's time to go then exit
      exit when Time_to_Go;
      ...
    end loop;

-------------------------
Exit Statement Examples
-------------------------

* Equivalent to C's :C:`do while`

   .. code:: Ada

      loop
        Do_Something;
        exit when Finished;
      end loop;

* Nested named loops and exit

   .. code:: Ada

      Outer : loop
        Do_Something;
        Inner : loop
          ...
          exit Outer when Finished; -- will exit all the way out
          ...
        end loop Inner;
      end loop Outer;

-----------------------
While-loop Statements
-----------------------

* Syntax

   .. code:: Ada

      while boolean_expression loop
         sequence_of_statements
      end loop;

* Identical to

   .. code:: Ada

      loop
         exit when not boolean_expression;
         sequence_of_statements
      end loop;

* Example

   .. code:: Ada

      while Count < Largest loop
        Count := Count + 2;
        Display (Count);
      end loop;

---------------------
For-loop Statements
---------------------

* One low-level form

   - General-purpose (looping, array indexing, etc.)
   - Explicitly specified sequences of values
   - Precise control over sequence

* Two high-level forms

   - Ada 2012
   - Focused on objects
   - Seen later with Arrays

-----------------
For in Statements
-----------------

* Successive values of a **discrete** type

   - eg. enumerations values

* Syntax

   .. code:: Ada

      for name in [reverse] discrete_subtype_definition loop
      ...
      end loop;

* Example

.. code:: Ada

     for Day in Days_T loop
        Refresh_Planning (Day);
     end loop;

.. container:: speakernote

   Name - loop parameter object
   Discrete subtype definition - loop parameter type and range of values

-----------------------------------
Variable and Sequence of Values
-----------------------------------

* Variable declared implicitly by loop statement

   - Has a view as constant
   - No assignment or update possible

* Initialized as :ada:`'First`, incremented as :ada:`'Succ`
* Syntactic sugar: several forms allowed

.. code:: Ada

   -- All values of a type or subtype
   for Day in Days_T loop
   for Day in Days_T range Mon .. Fri -- anonymous subtype
   -- Constant and variable range
   for Day in Mon .. Fri loop
   Today, Tomorrow : Days_T;
   ...
   for Day in Today .. Tomorrow loop

-----------------------------------
Low-Level For-loop Parameter Type
-----------------------------------

* The type can be implicit

   - As long as it is clear for the compiler
   - Warning: same name can belong to several enums

.. container:: latex_environment scriptsize

    .. code:: Ada
      :number-lines: 1

      procedure Main is
         type Color_T is (Red, White, Blue);
         type Rgb_T is (Red, Green, Blue);
      begin
         for Color in Red .. Blue loop  -- which Red and Blue?
            null;
         end loop;
         for Color in Rgb_T'(Red) .. Blue loop -- OK
            null;
         end loop;

    ::

      main.adb:5:21: error: ambiguous bounds in range of iteration
      main.adb:5:21: error: possible interpretations:
      main.adb:5:21: error: type "Rgb_T" defined at line 3
      main.adb:5:21: error: type "Color_T" defined at line 2
      main.adb:5:21: error: ambiguous bounds in discrete range

* If bounds are `universal_integer`, then type is :ada:`Integer` unless otherwise specified

   .. code:: Ada

      for Idx in 1 .. 3 loop -- Idx is Integer

      for Idx in Short range 1 .. 3 loop -- Idx is Short

-------------
Null Ranges
-------------

    * :dfn:`Null range` when lower bound ``>`` upper bound

       - :ada:`1 .. 0`, :ada:`Fri .. Mon`
       - Literals and variables can specify null ranges

    * No iteration at all (not even one)
    * Shortcut for upper bound validation

    .. code:: Ada

      -- Null range: loop not entered
      for Today in Fri .. Mon loop

-----------------------------------------
Reversing Low-Level Iteration Direction
-----------------------------------------

* Keyword :ada:`reverse` reverses iteration values

    - Range must still be ascending
    - Null range still cause no iteration

   .. code:: Ada

      for This_Day in reverse Mon .. Fri loop

---------------------------------------
For-Loop Parameter Visibility
---------------------------------------

* Scope rules don't change
* Inner objects can hide outer objects

   .. code:: Ada

      Block: declare
        Counter : Float := 0.0;
      begin
        -- For_Loop.Counter hides Block.Counter
        For_Loop : for Counter in Integer range A .. B loop
        ...
        end loop;
      end;

--------------------------
Referencing Hidden Names
--------------------------

* Must copy for-loop parameter to some other object if needed after the loop exits
* Use dot notation with outer scope name when hiding occurs

.. code:: Ada

   Foo:
   declare
      Counter : Float := 0.0;
   begin
      ...
      for Counter in Integer range 1 .. Number_Read loop
         -- set declared "Counter" to loop counter
         Foo.Counter := Float (Counter);
         ...
      end loop;
      ...
   end Foo;

--------------------------
Iterations Exit Statements
--------------------------

* Early loop exit
* Syntax

  .. code:: Ada

        exit [<loop_name>] [when <condition>]

* No name: Loop exited **entirely**

    - Not only current iteration

  .. code:: ada

     for K in 1 .. 1000 loop
        exit when K > F(K);
     end loop;

* With name: Specified loop exited

  .. code:: ada

     for J in 1 .. 1000 loop
         Inner: for K in 1 .. 1000 loop
            exit Inner when K > F(K);
         end loop;
     end loop;

--------------------------------------
For-Loop with Exit Statement Example
--------------------------------------

.. code:: Ada

   -- find position of Key within Table
   Found := False;
   -- iterate over Table
   Search : for Index in Table'Range loop
     if Table (Index) = Key then
       Found := True;
       Position := Index;
       exit Search;
     elsif Table (Index) > Key then
       -- no point in continuing
       exit Search;
     end if;
   end loop Search;

.. container:: speakernote

   We use the low-level for-loop form because we want to capture the actual position of the key within the table.

------
Quiz
------

.. code:: Ada

   A, B : Integer := 123;

Which loop block is **not** legal?

  A. | :answermono:`for A in 1 .. 10 loop`
     |    :answermono:`A := A + 1;`
     | :answermono:`end loop;`
  B. | ``for B in 1 .. 10 loop``
     |    ``Put_Line (Integer'Image (B));``
     | ``end loop;``
  C. | ``for C in reverse 1 .. 10 loop``
     |    ``Put_Line (Integer'Image (C));``
     | ``end loop;``
  D. | ``for D in 10 .. 1 loop``
     |    ``Put_Line (Integer'Image (D));``
     | ``end loop;``

.. container:: animate

   Explanations

   A. Cannot assign to a loop parameter
   B. Legal - 10 iterations
   C. Legal - 10 iterations
   D. Legal - 0 iterations

.

=================
GOTO Statements
=================

-----------------
GOTO Statements
-----------------

* Syntax

   .. code:: Ada

      goto_statement ::= goto label;
      label ::= << identifier >>

* Rationale

   - Historic usage
   - Arguably cleaner for some situations

* Restrictions

   - Based on common sense
   - Example: cannot jump into a `case` statement

--------
GOTO Use
--------

* Mostly discouraged
* May simplify control flow
* For example in-loop `continue` construct

.. code:: Ada

   loop
      -- lots of code
      ...
      goto continue;
      -- lots more code
      ...
      <<continue>>
   end loop;

* As always maintainability beats hard set rules

========
Lab
========

.. include:: labs/040_statements.lab.rst

=========
Summary
=========

---------
Summary
---------

* Assignments must satisfy any constraints of LHS

   - Invalid assignments don't alter target

* Intent to do nothing must be explicitly specified
* Case statements alternatives don't fall through
* Any kind of loop can be expressed with building blocks
