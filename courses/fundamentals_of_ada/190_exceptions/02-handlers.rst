==========
Handlers
==========

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
Similarity to Case Statements
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

.. code:: Ada

   begin
     ...
     raise Name3;
     -- code here is not executed
     ...
   exception
     when Name1 =>
        -- not executed
        ...
     when Name2 | Name3 =>
        -- executed
        ...
     when Name4 =>
        -- not executed
        ...
   end;

-----------------------------
When an Exception Is Raised
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
         A, B, C, D : Integer range 0 .. 100;
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

      A. Although :ada:`(A - C)` is not in the range of :ada:`Integer`, the range is only checked on assignment, which is after the addition of :ada:`B`, so :ada:`One` is never printed
      B. Correct
      C. If we reach :ada:`Two`, the assignment on line 16 will cause :ada:`Three` to be reached
      D. Divide by 0 on line 13 causes an exception, so :ada:`Two` must be called

