*********************
Advanced Exceptions
*********************

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

=============
Introduction
=============

-----------------
Advanced Usages
-----------------

* Language-defined exceptions raising cases
* Re-raising
* Raising and handling from elaboration
* Manipulating an exception with identity

    - Re-raising
    - Copying

==========
Handlers
==========

-----------------------------------------
Exceptions Raised in Exception Handlers
-----------------------------------------

.. container:: columns

 .. container:: column

    * Go immediately to caller unless also handled
    * Goes to caller in any case, as usual

 .. container:: column

    .. code:: Ada

       begin
         ...
       exception
         when Some_Error =>
           declare
             New_Data : Some_Type;
           begin
             P(New_Data);
             ...
           exception
             when ...
           end;
       end;

=============================
Language-Defined Exceptions
=============================

--------------------
`Constraint_Error`
--------------------

* Caused by violations of constraints on range, index, etc.
* The most common exceptions encountered

   .. code:: Ada

      K : Integer range 1 .. 10;
      ...
      K := -1;

   .. code:: Ada

      L : array (1 .. 100) of Some_Type;
      ...
      L (400) := SomeValue;

-----------------
`Program_Error`
-----------------

* When runtime control structure is violated

   - Elaboration order errors and function bodies

* When implementation detects bounded errors

   - Discussed momentarily

.. code:: Ada

   function F return Some_Type is
   begin
     if something then
       return Some_Value;
     end if; -- program error - no return statement
   end F;

-----------------
`Storage_Error`
-----------------

* When insufficient storage is available
* Potential causes

   - Declarations
   - Explicit allocations
   - Implicit allocations

.. code:: Ada

   Data : array (1..1e20) of Big_Type;

------------------------------
Explicitly-Raised Exceptions
------------------------------

.. container:: columns

 .. container:: column

    * Raised by application via :ada:`raise` statements

       - Named exception becomes active

    * Syntax

       .. code:: Ada

          raise_statement ::= raise; |
             raise exception_name
             [with string_expression];

       - :ada:`with string_expression` only available in Ada 2005 and later

    * A :ada:`raise` by itself is only allowed in handlers (more later)

 .. container:: column

    .. code:: Ada

       if Unknown (User_ID) then
         raise Invalid_User;
       end if;

       if Unknown (User_ID) then
         raise Invalid_User
            with "Attempt by " &
                 Image (User_ID);
       end if;

=============
Propagation
=============

-------------------------------
Partially Handling Exceptions
-------------------------------

.. container:: columns

 .. container:: column

    * Handler eventually re-raises the current exception
    * Achieved using :ada:`raise` by itself, since re-raising

       - Current active exception is then propagated to caller

 .. container:: column

    .. code:: Ada

       procedure Joy_Ride is
         ...
       begin
         while not Bored loop
           Steer_Aimlessly (Bored);
           Consume_Fuel (Hot_Rod);
         end loop;
       exception
         when Fuel_Exhausted =>
           Pull_Over;
           raise; -- no gas available
       end Joy_Ride;

----------------------------------
Typical Partial Handling Example
----------------------------------

* Log (or display) the error and re-raise to caller

   - Same exception or another one

.. code:: Ada

   procedure Get (Item : out Integer;   From : in File) is
   begin
     Ada.Integer_Text_IO.Get (From, Item);
   exception
     when Ada.Text_IO.End_Error =>
       Display_Error ("Attempted read past end of file");
       raise Error;
     when Ada.Text_IO.Mode_Error =>
       Display_Error ("Read from file opened for writing");
       raise Error;
     when Ada.Text_IO.Status_Error =>
       Display_Error ("File must be opened prior to use");
       raise Error;
     when others =>
       Display_Error ("Error in Get (Integer) from file");
       raise;
   end Get;

--------------------------------------
Exceptions Raised During Elaboration
--------------------------------------

* I.e., those occurring before the :ada:`begin`
* Go immediately to the caller
* No handlers in that frame are applicable

   - Could reference declarations that failed to elaborate!

.. code:: Ada

   procedure P (Output : out BigType) is
     -- storage error handled by caller
     N : array (Positive) of BigType;
     ...
   begin
     ...
   exception
     when Storage_Error =>
       -- failure to define N not handled here
       Output := N (1); -- if it was, this wouldn't work
       ...
   end P;

---------------------------------
Handling Elaboration Exceptions
---------------------------------

.. code:: Ada

   procedure Test is
     procedure P is
       X : Positive := 0;  -- Constraint_Error!
     begin
       ...
     exception
       when Constraint_Error =>
         Ada.Text_IO.Put_Line ("Got it in P");
     end P;
   begin
     P;
   exception
     when Constraint_Error =>
       Ada.Text_IO.Put_Line ("Got Constraint_Error in Test");
   end Test;

------
Quiz
------

.. container:: latex_environment footnotesize

   .. code:: Ada

      with Ada.Text_IO; use Ada.Text_IO;
      procedure Exception_Test (Input_Value : Integer) is
         Known_Problem : exception;
         function F (P : Integer) return Integer is
         begin
            if P > 0 then
               return P * P;
            end if;
         exception
            when others => raise Known_Problem;
         end F;
         procedure P (X : Integer) is
            A : array (1 .. F (X)) of Float;
         begin
            A := (others => 0.0);
         exception
            when others => raise Known_Problem;
         end P;
      begin
         P (Input_Value);
         Put_Line ("Success");
      exception
         when Known_Problem => Put_Line ("Known problem");
         when others => Put_Line ("Unknown problem");
      end Exception_Test;

What will get printed for these values of Input_Value?

.. list-table::

   * - **A.**

     - Integer'Last
     - :animate:`Known Problem`

   * - **B.**

     - Integer'First
     - :animate:`Unknown Problem`

   * - **C.**

     - 10000
     - :animate:`Unknown Problem`

   * - **D.**

     - 100
     - :animate:`Success`

.. container:: animate

   Explanations

   .. container:: latex_environment tiny

      A |rightarrow| When :ada:`F` is called with a large :ada:`P`, its own exception handler captures the exception and raises :ada:`Constraint_Error` (which the main exception handler processes)

      B/C |rightarrow| When the creation of :ada:`A` fails (due to :ada:`Program_Error` from passing :ada:`F` a negative number or :ada:`Storage_Error` from passing :ada:`F` a large number), then :ada:`P` raises an exception during elaboration, which is propagated to :ada:`Main`

=======================
Exceptions As Objects
=======================

------------------
Exceptions Scope
------------------

* Some differences for scope and visibility

   - May be propagated out of scope
   - Hidden predefined exceptions are still available
   - Are not dynamically allocated (unlike variables)

      + A rarely-encountered issue involving recursion

----------------------------------
Example Propagation Beyond Scope
----------------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       package P is
         procedure Q;
       end P;
       package body P is
         Error : exception;
         procedure Q is
         begin
           ...
           raise Error;
         end Q;
       end P;

 .. container:: column

    .. code:: Ada

       with P;
       procedure Client is
       begin
         P.Q;
       exception
         -- not visible
         when P.Error =>
            ...
         -- captured here
         when others =>
            ...
       end Client;

-----------------------------------
User Subprogram Parameter Example
-----------------------------------

.. code:: Ada

   with Ada.Exceptions; use Ada.Exceptions;
   procedure Display_Exception
       (Error : in Exception_Occurrence)
   is
     Msg : constant String := Exception_Message (Error);
     Info : constant String := Exception_Information (Error);
   begin
     New_Line;
     if Info /= "" then
       Put ("Exception information => ");
       Put_Line (Info);
     elsif Msg /= "" then
       Put ("Exception message => ");
       Put_Line (Msg);
     else
       Put ("Exception name => ");
       Put_Line (Exception_Name (Error));
     end if;
   end Display_Exception;

--------------------
Exception Identity
--------------------

* Attribute 'Identity converts exceptions to the type

   .. code:: Ada

      package Ada.Exceptions is
        ...
        type Exception_Id is private;
        ...
        procedure Raise_Exception (E : in Exception_Id;
                                   Message : in String := "");
        ...
      end Ada.Exceptions;

* Primary use is raising exceptions procedurally

   .. code:: Ada

      Foo : exception;
      ...
      Ada.Exceptions.Raise_Exception (Foo'Identity,
                                      Message => "FUBAR!");

------------------------------------
Re-Raising Exceptions Procedurally
------------------------------------

* Typical :ada:`raise` mechanism

   .. code:: Ada

      begin
        ...
      exception
        when others =>
          Cleanup;
          raise;
      end;

* Procedural :ada:`raise` mechanism

   .. code:: Ada

      begin
        ...
      exception
        when X : others =>
          Cleanup;
          Ada.Exceptions.Reraise_Occurrence (X);
      end;

----------------------------------------
Copying `Exception_Occurrence` Objects
----------------------------------------

* Via procedure `Save_Occurrence`

   - No assignment operation since is a :ada:`limited` type

.. code:: Ada

   Error : Exception_Occurrence;

   begin
     ...
   exception
     when X : others =>
       Cleanup;
       Ada.Exceptions.Save_Occurrence (X, Target => Error);
   end;

---------------------------------------
Re-Raising Outside Dynamic Call Chain
---------------------------------------

.. code:: Ada

   procedure Demo is
     package Exceptions is new
         Limited_Ended_Lists (Exception_Occurrence,
                              Save_Occurrence);
     Errors : Exceptions.List;
     Iteration : Exceptions.Iterator;
     procedure Normal_Processing
         (Troubles : in out Exceptions.List) is ...
   begin
     Normal_Processing (Errors);
     Iteration.Initialize (Errors);
     while Iteration.More loop
       declare
         Next_Error : Exception_Occurrence;
       begin
         Iteration.Read (Next_Error);
         Put_Line (Exception_Information (Next_Error));
         if Exception_Identity (Next_Error) =
            Trouble.Fatal_Error'Identity
         then
           Reraise_Occurrence (Next_Error);
         end if;
       end;
     end loop;
     Put_Line ("Done");
   end Demo;

=============
In Practice
=============

---------------------------------------
Fulfill Interface Promises to Clients
---------------------------------------

* If handled and not re-raised, normal processing continues at point of client's call
* Hence caller expectations must be satisfied

.. code:: Ada

   procedure Get (Reading : out Sensor_Reading) is
   begin
     ...
     Reading := New_Value;
     ...
   exceptions
     when Some_Error =>
       Reading := Default_Value;
   end Get;

   function Foo return Some_Type is
   begin
     ...
     return Determined_Value;
     ...
   exception
     when Some_Error =>
       return Default_Value; -- error if this isn't here
   end Foo;

-----------------------------------
Allow Clients to Avoid Exceptions
-----------------------------------

* Callee

   .. code:: Ada

      package Stack is
        Overflow : exception;
        Underflow : exception;
        function Full return Boolean;
        function Empty return Boolean;
        procedure Push (Item : in Some_Type);
        procedure Pop (Item : out Some_Type);
      end Stack;

* Caller

   .. code:: Ada

      if not Stack.Empty then
        Stack.Pop (...);  -- will not raise Underflow

----------------------------------
You Can Suppress Run-Time Checks
----------------------------------

* Syntax (could use a compiler switch instead)

   .. code:: Ada

      pragma Suppress (check-name [, [On =>] name]);

* Language-defined checks emitted by compiler
* Compiler may ignore request if unable to comply
* Behavior will be unpredictable if exceptions occur

   - Raised within the region of suppression
   - Propagated into region of suppression

.. code:: Ada

   pragma Suppress (Range_Check);
   pragma Suppress (Index_Check, On => Table);

-----------------------
Error Classifications
-----------------------

* Some errors must be detected at run-time

   - Corresponding to the predefined exceptions

* **Bounded Errors**

   - Need not be detected prior to/during execution if too hard
   - If not detected, range of possible effects is bounded

      + Possible effects are specified per error

   - Example: evaluating an un-initialized scalar variable
   - It might "work"!

* **Erroneous Execution**

   - Need not be detected prior to/during execution if too hard
   - If not detected, range of possible effects is not bounded
   - Example: Occurrence of a suppressed check

.. container:: speakernote

   Evaluation of an uninitialized scalar variable is a bounded error; evaluation of non-scalars is erroneous. See 13.9.1 Data Validity

========
Lab
========

.. include:: labs/adv_190_exceptions.lab.rst

=========
Summary
=========

---------
Summary
---------

* Re-raising exceptions is possible
* Suppressing checks is allowed but requires care

   - Testing only proves presence of errors, not absence
   - Exceptions may occur anyway, with unpredictable effects
