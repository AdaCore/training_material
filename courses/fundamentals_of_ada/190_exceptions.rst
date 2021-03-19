
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

------------------------
Exception Handler Part
------------------------

* Contains the exception handlers within a frame

   - Within block statements, subprograms, tasks, etc.

* Separates normal processing code from abnormal
* Starts with the reserved word `exception`
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
* If used, `others` must appear at the end, by itself

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
     
-----------------------------------------
Exceptions Raised In Exception Handlers
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
             P( New_Data );
             ...
           exception
             when ...
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
 
* Several checks and exceptions are language-defined

   - `Constraint_Error`
   - `Storage_Error`
   - `Program_Error`
   - Various others

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

.. container:: latex_environment scriptsize

 .. container:: columns

  .. container:: column
  
    * Raised by application via `raise` statements

       - Named exception becomes active

    * Syntax
    
       .. code:: Ada
    
          raise_statement ::= raise; |
             raise exception_name
             [with string_expression];

       - `with string_expression` only available in Ada 2005 and later
     
    * A `raise` by itself is only allowed in handlers (more later)

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

   - `raise` statements
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
           Print ("Exception from 3"); 
       end P;
     
-------------------
Termination Model
-------------------

* Control goes to handler (if any) and then to caller

   - Does not resume at point exception was raised

.. code:: Ada
    
   procedure Joy_Ride is
     Bored : Boolean := False;
   begin
     while not Bored loop
       Steer_Aimlessly (Bored);
       -- Exception in Consume_Fuel jumps to handler
       Consume_Fuel;
       -- Never called if Consume_Fuel raises exception
       Wave_To_Pedestrians;
     end loop;
     Drive_Home;
   exception
     when Fuel_Exhausted =>
       Push_Home;
       -- Completion of exception handler returns to caller
   end Joy_Ride;
     
-------------------------------
Partially Handling Exceptions
-------------------------------

.. container:: columns

 .. container:: column
  
    * Handler eventually re-raises the current exception
    * Achieved using `raise` by itself, since re-raising

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
       Display_Error ( "Error in Get(Integer) from file" );
       raise;
   end Get;
 
--------------------------------------
Exceptions Raised During Elaboration
--------------------------------------

* I.e., those occurring before the `begin`
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
      procedure Main is
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
         P ( Input_Value );
         Put_Line ( "Success" );
      exception
         when Known_Problem => Put_Line ("Known problem");
         when others => Put_Line ("Unknown problem");
      end Main;

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
Exceptions as Objects
=======================

----------
Examples
----------

.. include:: examples/190_exceptions/exceptions_as_objects.rst

----------------------------
Exceptions Are Not Objects
----------------------------

* May not be manipulated

   - May not be components of composite types
   - May not be passed as parameters
   - etc.

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
 
----------------------------------
Obtaining `Exception_Id` Objects
----------------------------------

* Attribute 'Identity converts exceptions to the type

   .. code:: Ada

      package Ada.Exceptions is
        ...
        type Exception_Id is private;
        ...
        procedure Raise_Exception( E : in Exception_Id; 
                                   Message : in String := "" );
        ...
      end Ada.Exceptions;
 
* Primary use is raising exceptions procedurally

   .. code:: Ada

      Foo : exception;
      ...
      Ada.Exceptions.Raise_Exception (Foo'Identity,
                                      Message => "FUBAR!");

------------------------------------------
Obtaining `Exception_Occurrence` Objects
------------------------------------------

* Syntax associates an object with active exception

   .. code:: Ada

      exception_handler ::=
         when [defining_identifier : ]
              exception_choice { | exception_choice } =>
            sequence_of_statements
      exception_choice ::= exception_name | others
 
* A constant view representing active exception
* Used with operations defined for the type

   .. code:: Ada

      ...
      exception
        when Error : others =>
          Put( "Bombed with " & Exception_Name(Error) );
      end;
 
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

------------------------------------
Re-Raising Exceptions Procedurally
------------------------------------

* Typical `raise` mechanism

   .. code:: Ada

      begin
        ...
      exception
        when others =>
          Cleanup;
          raise;
      end;
 
* Procedural `raise` mechanism

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

   - No assignment operation since is a `limited` type

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
 
=======================
*Raise Expressions*
=======================

-----------------------
*Raise Expressions*
-----------------------

.. admonition:: Language Variant

   Ada 2012

* Expressions, of type defined by enclosing context
* Evaluation at run-time raises specified exception
* Syntax mimics `raise` statements

   .. code:: Ada

      raise_expression ::=
         raise exception_name [with string_expression]
 
---------------------------
Using *Raise Expressions*
---------------------------

.. admonition:: Language Variant

   Ada 2012

* As parts of conditional expressions, but when an exception is appropriate too

.. code:: Ada
    
   procedure Demo (X : Integer) is
     Error : exception;
     M : Integer;
     Foo : constant Integer := ( case X is
                                 when 1 => 10,
                                 when 2 => 20,
                                 when others => raise Error);
    begin
     M := (if Foo = 10 then 100 else
           raise Error with "Foo is not 10!");
     ...
   end Demo;
     
=============
In Practice
=============

---------------------------------------
Fulfill Interface Promises To Clients
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
Allow Clients To Avoid Exceptions
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
        Stack.Pop( ... );  -- will not raise Underflow
      ...
 
---------------------------------------
Exceptions Are Not Always Appropriate
---------------------------------------

.. container:: columns

 .. container:: column
  
    * What does it mean to have an unexpected error in a safety-critical application?

       - Maybe there's no reasonable response

 .. container:: column
  
    .. image:: ../../images/airbag_exception_handler.png
    
----------------------------------
You Can Suppress Run-Time Checks
----------------------------------

* Syntax (could use a compiler switch instead)

   .. code:: Ada

      pragma Suppress ( check-name [, [On =>] name] );
 
* Language-defined checks emitted by compiler
* Compiler may ignore request if unable to comply
* Behavior will be unpredictable if exceptions occur

   - Raised within the region of suppression
   - Propagated into region of suppression

.. code:: Ada

   pragma Suppress (Range_Check);
   pragma Suppress (Index_Check, On => Table);
 
---------------------------------------
Relying On Exception Raising Is Risky
---------------------------------------

* They may be suppressed!
* Risky

   .. code:: Ada

      function Tomorrow (Today : Days) return Days is
      begin
        return Days'Succ (Today);
      exception
        when Constraint_Error =>
          return Days'First;
      end Tomorrow;
 
* Better

   .. code:: Ada

      function Tomorrow (Today : Days) return Days is
      begin
        if Today = Days'Last then
          return Days'First;
        else
          return Days'Succ (Today);
        end if;
      end Tomorrow;
 
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

.. include:: labs/190_exceptions.lab.rst

=========
Summary
=========

---------
Summary
---------

* Should be for unexpected errors
* Give clients the ability to avoid them
* If handled, caller should see normal effect

   - Mode `out` parameters assigned
   - Function return values provided

* Package `Ada.Exceptions` provides views as objects

   - For both raising and special handling
   - Especially useful for debugging

* Suppressing checks is allowed but requires care

   - Testing only proves presence of errors, not absence
   - Exceptions may occur anyway, with unpredictable effects
