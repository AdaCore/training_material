=============================
Partial and Nested Handlers
=============================

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
