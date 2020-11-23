
************
Statements
************

==============
Introduction
==============

-----------------
Statement Kinds
-----------------

.. code:: Ada

   simple_statement ::=
     null_statement | assignment_statement | exit_statement |
     goto_statement | delay_statement | raise_statement |
     procedure_call_statement | return_statement |
     requeue_statement | entry_call_statement |
     abort_statement | code_statement

   compound_statement ::=
     if_statement | case_statement | loop_statement |
     block_statement | accept_statement | select_statement

----------------------------
Procedure Calls (Overview)
----------------------------

    * Procedure calls are statements as shown here
    * More details in "Subprograms" section

    .. code:: Ada

       procedure Activate ( This : in out Foo; Wait : in Boolean);

    * Traditional call notation is supported

    .. code:: Ada

       Activate (Idle, True);

    * "Distinguished Receiver" notation is supported

    .. code:: Ada

       Idle.Activate (True);


---------------------------------
Parameter Associations In Calls
---------------------------------

* Traditional "positional association" is allowed

   - Nth actual parameter goes to nth formal parameter

.. code:: Ada

   Activate ( Idle, True ); -- positional

* "Named association" also allowed

   - Name of formal parameter is explicit

.. code:: Ada

   Activate ( This => Idle, Wait => True ); -- named

* Both can be used together

.. code:: Ada

   Activate ( Idle, Wait => True ); -- named then positional

* But positional following named is a compile error

.. code:: Ada

   Activate ( This => Idle, True ); -- ERROR

==================
Block Statements
==================

------------------
Block Statements
------------------

    * "Sort of" inline procedures
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

      assignment_statement ::= variable_name := expression;

* Value of expression is copied to target variable
* The type of the RHS must be same as the LHS

   - Rejected at compile-time otherwise

.. code:: Ada

   type Bar is range 0 .. Max;
   type Foo is range -200 .. 200;
   ...
   F : Foo := 2; -- universal integer legal for any integer
   B : Bar := 2; -- universal integer legal for any integer
   F := B; -- compile error

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

   - E.g. `if ( a == 1 )` compared to `if ( a = 1 )`

------------------
Assignable Views
------------------

* Views control the way an entity can be treated

   - At different points in the program text

* The named entity must be an assignable variable

   - Thus the view of the target object must allow assignment

* Various un-assignable views

   - Constants
   - Variables of `limited` types
   - Formal parameters of mode `in`

.. code:: Ada

   Max : constant Integer := 100;
   ...
   Max := 200; -- illegal

---------------------------------------
Target Variable Constraint Violations
---------------------------------------

* Prevent update to target value

   - Target is not changed at all

* May compile but will raise error at runtime

   - Predefined exception `Constraint_Error` is raised

* May be detected by compiler

   - Static value
   - Value is outside base range of type

.. code:: Ada

   Max : Integer range 1 .. 100 := 100;
   ...
   Max := 0; -- run-time error

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

---------------------------------
Not All Assignments Are Checked
---------------------------------

* Compilers assume variables of a subtype have appropriate values
* No check generated in this code

   .. code:: Ada

      procedure Demo is
        P, K : Integer range 0 .. 100;
      begin
        ...
        P := K;
        ...
      end Demo;

========================
Conditional Statements
========================

--------------------
If-then Statements
--------------------

* Control flow using Boolean expressions
* Syntax

   .. code:: Ada

      if boolean_expression then -- Note: No parentheses
         sequence_of_statements;
      end if;

* At least one statement must be supplied (`null` for explicit no-op)

    .. code:: Ada

       if Valve(N) /= Closed then
         Isolate( Valve(N) );
         Notify( Valve_Failure,
                 Valve(N) );
       end if;

-------------------------
If-then-else Statements
-------------------------

* Express exclusionary choice
* Syntax

   .. code:: Ada

      if boolean_expression then
         sequence_of_statements;
      else
         sequence_of_statements;
      end if;

    .. code:: Ada

         if Today = Days'Last then
           Next := Days'First;
         else -- Today is not the last day
           Next := Days'Succ(Today);
         end if;

--------------------------
If-then-elsif Statements
--------------------------

    * Sequential choice with alternatives
    * Avoids `if` nesting
    * `elsif` alternatives, tested in textual order
    * `else` part still optional

    .. code:: Ada

       if A = 0 then
         Put_Line ("Zero");
       elsif A < 10 then
         Put_Line ("Small");
       elsif A < 100  then
         Put_Line ("Medium");
       -- other `elsif` if needed
       else
         Put_Line("Large");
       end if;

.. container:: speakernote

   Spelled that way on purpose, as was done in Python for example (differently, "elif")

------------------------------
Nested If-then Version (Ugly!)
------------------------------

* Doesn't scale well
* Doesn't read well either

.. code:: Ada

       if A = 0 then
         Put_Line ("Zero");
       else
          if A < 10 then
             Put_Line ("Small");
          else
             if A < 100 then
                Put_Line ("Medium");
             else
                Put_Line("Large");
             end if;
          end if;
       end if;

-----------------
Case Statements
-----------------

* Exclusionary choice among alternatives
* Syntax

   .. code:: Ada

      case expression is
        case_statement_alternative
        { case_statement_alternative }
      end case;
      case_statement_alternative ::= 
         when discrete_choice { | discrete_choice } =>
            sequence_of_statements
      discrete_choice ::= expression
                          | discrete_range
                          | others
 
----------------------
Simple case Statements
----------------------

.. code:: Ada

   type Directions is  (Forward, Backward, Left, Right);
   Direction : Directions;
   ...
   case Direction is
     when Forward =>  Go_Forward (1);
     when Backward => Go_Backward (1);
     when Left =>  Go_Left (1);
     when Right => Go_Right (1);
   end case;
 
* NB: There is no fall-through between cases 

----------------------
Case Statement Rules
----------------------

* More constrained than a if-elsif structure
* All possible values must be covered

   - Explicitly
   - ... or with `others` keyword

* Choice values cannot be given more than once (exclusive)

    - and should be known at compile-time

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
    
    - Compiler won't warn: `others` hides it

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
    
      loop_statement ::= [loop_simple_name:]   
        [iteration_scheme] loop
           sequence_of_statements
        end loop [loop_simple_name];
     
      iteration_scheme ::= while boolean_expression
                           | for <loop_parameter_specification>
                           | for <loop_iterator_specification>
     
* Example

   .. code:: Ada
       
      Wash_Hair : loop
        Lather;
        Rinse;
      end loop Wash_Hair;

.. container:: speakernote

    Loop Iterator Specification available in Ada2012 and later

-----------------
Exit Statements
-----------------

* Leaves innermost loop

   - Unless loop name is specified

* Syntax
    
   .. code:: Ada
    
      exit_statement ::= exit
         [loop_name]
         [when boolean_expression];
     
* `exit when` exits with condition

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

* Equivalent to a "do while" loop

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

.. admonition:: Language Variant
    
    Ada 2012

* One low-level form

   - General-purpose (looping, array indexing, etc.)
   - Explicitly specified sequences of values
   - Precise control over sequence

* Two high-level forms

   - Focused on objects
   - Seen with Arrays

-------------------------------
Low-Level For-loop Statements
-------------------------------

.. admonition:: Language Variant
    
    Ada 2012

* Take on successive values of a discrete type

   - eg. values of an enum

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

.. admonition:: Language Variant
    
    Ada 2012

* Variable declared implicitly by loop statement

   - Has a view as constant
   - No assignment or update possible

* Initialized as the `'First`, icremented as the `'Next`

* Syntaxic sugar: several forms allowed

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

.. admonition:: Language Variant
    
    Ada 2012

* The type can be implicit 

   - As long as it is clear for the compiler
   - Warning: same name can belong to several enums

   .. code:: Ada

      -- Error if Wed is both Days_T and Marital_Status_T
      for Today in Wed .. Fri loop  

* Type `Integer` by default

   - Each bound must be a `universal_integer`

-------------
Null Ranges
-------------

    * Null range when lower bound ``>`` upper bound

       - `1 .. 0`, `Fri .. Mon`
       - Litterals and variables can specify null ranges

    * Cause no iteration at all (not even one)
    * Nice shortcut for upper bound validation

    .. code:: Ada
    
       -- if Items_To_Get is 0, loop is not entered
       for K in 1 .. Items_To_Get loop
           Get (Item);
       end loop;

-----------------------------------------
Reversing Low-Level Iteration Direction
-----------------------------------------

* Keyword `reverse` reverses iteration values

    - Range must still be ascending

   .. code:: Ada

      for This_Day in reverse Mon .. Fri loop

* `Reverse` on a null range also cause no iteration

---------------------------------------
For-Loop Parameter Visibility
---------------------------------------

* Scope rules don't change
* Inner objects can hide outer objects

   .. code:: Ada

      Block: declare
        Counter : Integer := 0;
      begin
        -- For_Loop.Counter hides Block.Counter
        For_Loop : for Counter in Integer range A .. B loop
        ...
        end loop;

------------------------------------------
Combining Iterations and Exit Statements
------------------------------------------

    * Early loop exit
    * Loop exited entirely
        
        - Not only current iteration

    .. code:: Ada

       for K in 1 .. 1000 loop
          exit when K > F(K);
       end loop;

--------------------------------------
For-Loop with Exit Statement Example
--------------------------------------

.. code:: Ada

   -- find position of Key within Table
   Found := False;
   -- iterate over Table
   Search : for Index in Table'Range loop
     if Table(Index) = Key then
       Found := True;
       Position := Index;
       exit Search;
     elsif Table(Index) > Key then
       -- no point in continuing
       exit Search;
     end if;
   end loop Search;

.. container:: speakernote

   We use the low-level for-loop form because we want to capture the actual position of the key within the table.


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

* Mostly forbidden
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

* As always maintenability beats hard set rules

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
