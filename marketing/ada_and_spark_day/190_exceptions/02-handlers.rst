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

**Example**

.. code:: Ada

   begin
     Counter := Counter + 1;
   exception
      when Constraint_Error =>
         Log_Error ("Overflow");
   end;
   
---------------------------
Exception Handlers Syntax
---------------------------

* Associates exception names with statements to execute in response
* If used, :ada:`others` must appear at the end, by itself

   - Associates statements with all other exceptions

**Syntax**

.. code:: bnf

  begin
    sequence_of_statements
  [ exception
      exception_handler
      { exception handler } ]
  end

.. code:: bnf

  exception_handler ::= 
    when [identifier:] exception_choice
                       {'|' exception_choice} =>
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

* Normal processing is abandoned
* Handler for active exception is executed, if any
* Control then goes to the caller
* If handled, caller continues normally, otherwise repeats the above

**Caller**

  .. code:: Ada

    ...
    Joy_Ride;
    Do_Something_At_Home;
    ...

**Callee**

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

---------------------------
Exception Handler Content
---------------------------

* No restrictions

  - Block statements, subprogram calls, etc.

* Do whatever makes sense

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
