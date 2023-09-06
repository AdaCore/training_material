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

----------------------------
Procedure Calls (Overview)
----------------------------

    * Procedure calls are statements as shown here
    * More details in "Subprograms" section

    .. code:: Ada

       procedure Activate (This : in out Foo; Wait : in Boolean);

    * Traditional call notation

      .. code:: Ada

        Activate (Idle, True);

    * "Distinguished Receiver" notation

        - For :ada:`tagged` types

      .. code:: Ada

         Idle.Activate (True);

---------------------------------
Parameter Associations In Calls
---------------------------------

* Traditional :dfn:`positional association` is allowed

   - Nth actual parameter goes to nth formal parameter

.. code:: Ada

   Activate (Idle, True); -- positional

* :dfn:`Named association` also allowed

   - Name of formal parameter is explicit

.. code:: Ada

   Activate (This => Idle, Wait => True); -- named

* Both can be used together

.. code:: Ada

   Activate (Idle, Wait => True); -- positional then named

* But positional following named is a compile error

.. code:: Ada

   Activate (This => Idle, True); -- ERROR

==================
Block Statements
==================

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

.. code:: Ada

   type Miles_T is range 0 .. Max_Miles;
   type Km_T is range 0 .. Max_Kilometers
   ...
   M : Miles_T := 2; -- universal integer legal for any integer
   K : Km_T := 2; -- universal integer legal for any integer
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

------------------------------------
Implicit Range Constraint Checking
------------------------------------

* The following code

   .. code:: Ada

      procedure Demo is
        K : Integer;
        P : Integer range 0 .. 100;
      begin
        ...
        P := K;
        ...
      end Demo;

* Generates assignment checks similar to

   .. code:: Ada

      if K < 0 or K > 100 then
        raise Constraint_Error;
      else
        P := K;
      end if;

* Run-time performance impact

========================
Conditional Statements
========================

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

     if Valve(N) /= Closed then
       Isolate (Valve(N));
       Failure (Valve (N));
     else
       if System = Off then
         Failure (Valve (N));
       end if;
     end if;

 .. container:: column

  .. code:: Ada
     :number-lines: 1

     if Valve(N) /= Closed then
       Isolate (Valve(N));
       Failure (Valve (N));
     elsif System = Off then
       Failure (Valve (N));
     end if;

.. container:: speakernote

   Spelled that way on purpose, as was done in Python for example (differently, "elif")

----------------------
Simple case Statements
----------------------

.. code:: Ada

   type Directions is  (Forward, Backward, Left, Right);
   Direction : Directions;
   ...
   case Direction is
     when Forward =>
       Set_Mode (Drive);
       Go_Forward (1);
     when Backward =>
       Set_Mode (Reverse);
       Go_Backward (1);
     when Left =>
       Go_Left (1);
     when Right =>
       Go_Right (1);
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

=================
Loop Statements
=================

------------------------
Basic Loops and Syntax
------------------------

* All kind of loops can be expressed

  - Optional iteration controls
  - Optional exit statements

* Example

   .. code:: Ada

      Wash_Hair : loop
        Lather (Hair);
        Rinse (Hair);
      end loop Wash_Hair;

.. container:: speakernote

    Loop Iterator Specification available in Ada2012 and later

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
