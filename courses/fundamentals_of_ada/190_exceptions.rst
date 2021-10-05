
************
Exceptions
************

.. role:: ada(code)
    :language: Ada

.. |rightarrow| replace:: :math:`\rightarrow`

==============
Introduction
==============

--------------------------
Rationale for Exceptions
--------------------------

* Textual separation from normal processing
* Rigorous Error Management

   - Cannot be ignored, unlike status codes from routines
   - Example: running out of gasoline in an automobile

.. code:: Ada

   package Automotive is
     type Vehicle is record
       Fuel_Quantity, Fuel_Minimum : Float;
       Oil_Temperature : Float;
       ...
     end record;
     Fuel_Exhausted : exception;
     procedure Consume_Fuel (Car : in out Vehicle);
     ...
   end Automotive;

--------------------
Semantics Overview
--------------------

* Exceptions become active by being *raised*

   - Failure of implicit language-defined checks
   - Explicitly by application

* Exceptions occur at run-time

   - A program has no effect until executed

* May be several occurrences active at same time

   - One per thread of control

* Normal execution abandoned when they occur

   - Error processing takes over in response
   - Response specified by **exception handlers**
   - *Handling the exception* means taking action in response
   - Other threads need not be affected

----------------------------
Semantics Example: Raising
----------------------------

.. code:: Ada

   package body Automotive is
     function Current_Consumption return Float is
       ...
     end Current_Consumption;
     procedure Consume_Fuel (Car : in out Vehicle) is
     begin
       if Car.Fuel_Quantity <= Car.Fuel_Minimum then
         raise Fuel_Exhausted;
       else -- decrement quantity
         Car.Fuel_Quantity := Car.Fuel_Quantity -
                              Current_Consumption;
       end if;
     end Consume_Fuel;
     ...
   end Automotive;

-----------------------------
Semantics Example: Handling
-----------------------------

.. code:: Ada

   procedure Joy_Ride is
     Hot_Rod : Automotive.Vehicle;
     Bored : Boolean := False;
     use Automotive;
   begin
     while not Bored loop
       Steer_Aimlessly (Bored);
       -- error situation cannot be ignored
       Consume_Fuel (Hot_Rod);
     end loop;
     Drive_Home;
   exception
     when Fuel_Exhausted =>
       Push_Home;
   end Joy_Ride;

.. container:: speakernote

   Cannot ignore exception (someone needs to handle it)

---------------------------------------
Handler Part Is Skipped Automatically
---------------------------------------

* If no exceptions are active, returns normally

.. code:: Ada

   begin
     ...
   -- if we get here, skip to end
   exception
     when Name1 =>
     ...
     when Name2 | Name3 =>
     ...
     when Name4 =>
     ...
   end;

==========
Handlers
==========

----------
Examples
----------

.. include:: examples/190_exceptions/handlers.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/190_exceptions.html#handlers`

------------------------
Exception Handler Part
------------------------

* Contains the exception handlers within a frame

   - Within block statements, subprograms, tasks, etc.

* Separates normal processing code from abnormal
* Starts with the reserved word :ada:`exception`
* Optional

   .. code:: Ada

      begin
        sequence_of_statements
      [ exception
          exception_handler
          { exception handler } ]
      end

---------------------------
Exception Handlers Syntax
---------------------------

* Associates exception names with statements to execute in response
* If used, :ada:`others` must appear at the end, by itself

   - Associates statements with all other exceptions

* Syntax

   .. code:: Ada

      exception_handler ::=
        when exception_choice { | exception_choice } =>
          sequence_of_statements
      exception_choice ::= exception_name | others

-------------------------------
Similarity To Case Statements
-------------------------------

* Both structure and meaning
* Exception handler

   .. code:: Ada

      ...
      exception
        when Constraint_Error | Storage_Error | Program_Error =>
        ...
        when others =>
        ...
      end;

* Case statement

   .. code:: Ada

      case exception_name is
        when Constraint_Error | Storage_Error | Program_Error =>
        ...
        when others =>
        ...
      end case;

-------------------------------
Handlers Don't "Fall Through"
-------------------------------

* Again as in case statements

.. code:: Ada

   begin
     ...
     -- sequence of statements
     -- a statement causes Name3 to be raised
     ...
     -- so code here is not executed
     ...
   exception
     when Name1 =>
       ...
       -- not executed
       ...
     when Name2 | Name3 =>
     ...
     -- executed
     ...
     when Name4 =>
       ...
       -- not executed
       ...
   end;

-----------------------------
When An Exception Is Raised
-----------------------------

.. container:: columns

 .. container:: column

    * Normal processing is abandoned
    * Handler for active exception is executed, if any
    * Control then goes to the caller
    * If handled, caller continues normally, otherwise repeats the above

 .. container:: column

    * Caller

       .. code:: Ada

          ...
          Joy_Ride;
          Do_Something_At_Home;
          ...

    * Callee

       .. code:: Ada

          procedure Joy_Ride is
            ...
          begin
            ...
            Drive_Home;
          exception
            when Fuel_Exhausted =>
              Push_Home;
          end Joy_Ride;

.. container:: speakernote

   In this case, Caller does not know the Joy Ride ran out of gas

------------------------------------------
Handling Specific Statements' Exceptions
------------------------------------------

.. code:: Ada

   begin
     loop
       Prompting : loop
         Put (Prompt);
         Get_Line (Filename, Last);
         exit when Last > Filename'First - 1;
       end loop Prompting;
       begin
         Open (F, In_File, Filename (1..Last));
         exit;
       exception
         when Name_Error =>
           Put_Line ("File '" & Filename (1..Last) &
                     "' was not found.");
       end;
     end loop;

.. container:: speakernote

   Opens the Ada.TextIO file named via the user in the prompt.
   The call to Open will raise Ada.TextIO.NameError if the specified file cannot be found.

---------------------------
Exception Handler Content
---------------------------

.. container:: columns

 .. container:: column

    * No restrictions

       - Block statements, subprogram calls, etc.

    * Do whatever makes sense

 .. container:: column

    .. code:: Ada

       begin
         ...
       exception
         when Some_Error =>
           declare
             New_Data : Some_Type;
           begin
             P (New_Data);
             ...
           end;
       end;

------
Quiz
------

.. container:: latex_environment scriptsize

 .. container:: columns

  .. container:: column

   .. code:: Ada
    :number-lines: 1

      procedure Main is
         A, B, C, D : Natural;
      begin
         A := 1; B := 2; C := 3; D := 4;
         begin
            D := A - C + B;
         exception
            when others => Put_Line ("One");
                           D := 1;
         end;
         D := D + 1;
         begin
            D := D / (A - C + B);
         exception
            when others => Put_Line ("Two");
                           D := -1;
         end;
      exception
         when others =>
            Put_Line ("Three");
      end Main;

  .. container:: column

   What will get printed?

      A. One, Two, Three
      B. :answer:`Two, Three`
      C. Two
      D. Three

   .. container:: animate

      Explanations

      A. Although :ada:`(A - C)` is not in the range of :ada:`natural`, the range is only checked on assignment, which is after the addition of :ada:`B`, so :ada:`One` is never printed
      B. Correct
      C. If we reach :ada:`Two`, the assignment on line 10 will cause :ada:`Three` to be reached
      D. Divide by 0 on line 14 causes an exception, so :ada:`Two` must be called

=============================================
Implicitly and Explicitly Raised Exceptions
=============================================

----------
Examples
----------

.. include:: examples/190_exceptions/implicitly_and_explicitly_raised_exceptions.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/190_exceptions.html#implicitly-and-explicitly-raised-exceptions`

------------------------------
Implicitly-Raised Exceptions
------------------------------

* Correspond to language-defined checks
* Can happen by statement execution

   .. code:: Ada

      K := -10;  -- where K must be greater than zero

* Can happen by declaration elaboration

   .. code:: Ada

      Doomed : array (Positive) of Big_Type;

----------------------------------
Some Language-Defined Exceptions
----------------------------------

* :ada:`Constraint_Error`

    - Violations of constraints on range, index, etc.

* :ada:`Program_Error`

    - Runtime control structure violated (function with no return ...)

* :ada:`Storage_Error`

    - Insufficient storage is available

* For a complete list see RM Q-4

------------------------------
Explicitly-Raised Exceptions
------------------------------

.. container:: latex_environment scriptsize

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

    * A :ada:`raise` by itself is only allowed in handlers

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

=========================
User-Defined Exceptions
=========================

----------
Examples
----------

.. include:: examples/190_exceptions/user_defined_exceptions.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/190_exceptions.html#user-defined-exceptions`

-------------------------
User-Defined Exceptions
-------------------------

* Syntax

   .. code:: Ada

      defining_identifier_list : exception;

* Behave like predefined exceptions

   - Scope and visibility rules apply
   - Referencing as usual
   - Some minor differences

* Exception identifiers' use is restricted

   - :ada:`raise` statements
   - Handlers
   - Renaming declarations

---------------------------------
User-Defined Exceptions Example
---------------------------------

* An important part of the abstraction
* Designer specifies how component can be used

.. code:: Ada

   package Stack is
     Underflow, Overflow : exception;
     procedure Push (Item : in Integer);
     procedure Pop (Item : out Integer);
     ...
   end Stack;

   package body Stack is
     procedure Push (Item : in Integer) is
     begin
       if Top = Index'Last then
         raise Overflow;
       end if;
       Top := Top + 1;
       Values (Top) := Item;
     end Push;

     procedure Pop (Item : out Integer) is
     begin
       if Top = 0 then
         raise Underflow;
       end if;
       Item := Values (Top);
       Top := Top - 1;
     end Pop;
   end Stack;

=============
Propagation
=============

----------
Examples
----------

.. include:: examples/190_exceptions/propagation.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/190_exceptions.html#propagation`

-------------
Propagation
-------------

* Control does not return to point of raising

   - Termination Model

* When a handler is not found in a block statement

   - Re-raised immediately after the block

* When a handler is not found in a subprogram

   - Propagated to caller at the point of call

* Propagation is dynamic, back up the call chain

   - Not based on textual layout or order of declarations

* Propagation stops at the main subprogram

   - Main completes abnormally unless handled

------------------
Propagation Demo
------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       procedure P is
         Error : exception;
         procedure R is
         begin
           Maybe_Raise(1);
         end R;
         procedure Q is
         begin
           R;
           Maybe_Raise(2);
         exception
           when Error =>
             Print("Exception from 1 or 2");
         end Q;

 .. container:: column

    .. code:: Ada

       begin
         Maybe_Raise(3);
         Q;
       exception
         when Error =>
           Print("Exception from 3");
       end P;

-------------------
Termination Model
-------------------

* When control goes to handler, it continues from here

.. code:: Ada

   procedure Joy_Ride is
   begin
      loop
          Steer_Aimlessly;

          -- If next line raises Fuel_Exhausted, go to handler
          Consume_Fuel;
      end loop;
   exception
     when Fuel_Exhausted => -- Handler
       Push_Home;
       -- Resume from here: loop has been exited
   end Joy_Ride;

------
Quiz
------

.. container:: latex_environment footnotesize

   .. code:: Ada

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Main is
       Main_Problem : exception;
       function F (P : Integer) return Integer is
       begin
          if P > 0 then
             return P + 1;
          elsif P = 0 then
             raise Main_Problem;
          end if;
       end F;

       procedure P (X : Integer) is
          R : Integer;
       begin
          R := F (X);
       end P;
    begin
       P ( 0 );
       Put_Line ( "Success" );
    exception
       when Constraint_Error => Put_Line ("Constraint Error");
       when Program_Error => Put_Line ("Program Error");
       when others => Put_Line ("Unknown problem");
    end Main;

What will get printed for these values of Input_Value?

.. list-table::

   * - **A.**
     - Integer'Last
     - :animate:`Constraint Error`
   * - **B.**
     - 0
     - :animate:`Unknown problem`
   * - **C.**
     - Integer'First
     - :animate:`Program Error`
   * - **D.**
     - 100
     - :animate:`Success`

.. container:: animate

   Explanations

   .. container:: latex_environment tiny

      A |rightarrow| When :ada:`F` is called with :ada:`Integer'Last`, it overflows and raises a :ada:`Constraint_Error`

      B |rightarrow| The :ada:`Main_Problem` exception is raised, and catched in the :ada:`when others`
      C |rightarrow| :ada:`function F` does not hit return, a :ada:`Program_Error` is raised

=======================
Exceptions as Objects
=======================

----------
Examples
----------

.. include:: examples/190_exceptions/exceptions_as_objects.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/190_exceptions.html#exceptions-as-objects`

----------------------------
Exceptions Are Not Objects
----------------------------

* May not be manipulated

   - May not be components of composite types
   - May not be passed as parameters

* Some differences for scope and visibility

   - May be propagated out of scope

-----------------------------------
But You Can Treat Them As Objects
-----------------------------------

* For raising and handling, and more
* Standard Library

.. code:: Ada

   package Ada.Exceptions is
     type Exception_Id is private;
     procedure Raise_Exception (E : Exception_Id;
                                Message : String := "");
     ...
     type Exception_Occurrence is limited private;
     function Exception_Name (X : Exception_Occurrence)
         return String;
     function Exception_Message (X : Exception_Occurrence)
         return String;
     function Exception_Information (X : Exception_Occurrence)
         return String;
     procedure Reraise_Occurrence (X : Exception_Occurrence);
     procedure Save_Occurrence (
       Target : out Exception_Occurrence;
       Source : Exception_Occurrence);
     ...
   end Ada.Exceptions;

---------------------
Exception Occurence
---------------------

* Syntax associates an object with active exception

   .. code:: Ada

      when defining_identifier : exception_name ... =>

* A constant view representing active exception
* Used with operations defined for the type

   .. code:: Ada

      exception
        when Catched_Exception : others =>
          Put (Exception_Name (Catched_Exception));

----------------------------------------
`Exception_Occurrence` Query Functions
----------------------------------------

* `Exception_Name`

   - Returns full expanded name of the exception in string form

      + Simple short name if space-constrained

   - Predefined exceptions appear as just simple short name

* `Exception_Message`

   - Returns string value specified when raised, if any

* `Exception_Information`

   - Returns implementation-defined string content
   - Should include both exception name and message content
   - Presumably includes debugging information

      + Location where exception occurred
      + Language-defined check that failed (if such)

=======================
*Raise Expressions*
=======================

-----------------------
*Raise Expressions*
-----------------------

.. admonition:: Language Variant

   Ada 2012

* **Expression** raising specified exception **at run-time**

.. code:: Ada

    Foo : constant Integer := ( case X is
                                when 1 => 10,
                                when 2 => 20,
                                when others => raise Error);

=============
In Practice
=============

---------------------------------------
Exceptions Are Not Always Appropriate
---------------------------------------

.. container:: columns

 .. container:: column

    * What does it mean to have an unexpected error in a safety-critical application?

       - Maybe there's no reasonable response

 .. container:: column

    .. image:: airbag_exception_handler.png

---------------------------------------
Relying On Exception Raising Is Risky
---------------------------------------

* They may be **suppressed**
* Not recommended

   .. code:: Ada

      function Tomorrow (Today : Days) return Days is
      begin
        return Days'Succ (Today);
      exception
        when Constraint_Error =>
          return Days'First;
      end Tomorrow;

* Recommended

   .. code:: Ada

      function Tomorrow (Today : Days) return Days is
      begin
        if Today = Days'Last then
          return Days'First;
        else
          return Days'Succ (Today);
        end if;
      end Tomorrow;

=========
Summary
=========

---------
Summary
---------

* Should be for unexpected errors
* Give clients the ability to avoid them
* If handled, caller should see normal effect

   - Mode :ada:`out` parameters assigned
   - Function return values provided

* Package `Ada.Exceptions` provides views as objects

   - For both raising and special handling
   - Especially useful for debugging

* Checks may be suppressed
