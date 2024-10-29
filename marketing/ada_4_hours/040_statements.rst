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

      type Miles_T is range 0 .. Max_Miles;
      type Km_T is range 0 .. Max_Kilometers
      ...
      M : Miles_T := 2; -- universal integer legal for any integer
      K : Km_T := 2; -- universal integer legal for any integer
      ...
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

----------------------------
Procedure Calls
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

------------------
Block Statements
------------------

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

========================
Conditional Statements
========================

----------------------------
"If-then-elsif" Statements
----------------------------

* Sequential choice with alternatives
* Avoids :ada:`if` nesting
* :ada:`elsif` alternatives, tested in textual order
* :ada:`else` part still optional

.. container:: columns

 .. container:: column

  .. code:: Ada

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

     if Valve(N) /= Closed then
       Isolate (Valve(N));
       Failure (Valve (N));
     elsif System = Off then
       Failure (Valve (N));
     end if;

.. container:: speakernote

   Spelled that way on purpose, as was done in Python for example (differently, "elif")

-----------------------
"Case" Statements
-----------------------

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

=================
Loop Statements
=================

------------------------
Basic Loops
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

-------------------------
"while-loop" Statements
-------------------------

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
"for-in" Statements
---------------------

* Successive values of a **discrete** type

   - eg. enumerations values

* Example

.. code:: Ada

     for Day in Days_T loop
        Refresh_Planning (Day);
     end loop;

     for Idx in reverse 1 .. 10 loop
        Countdown (Idx);
     end loop;
