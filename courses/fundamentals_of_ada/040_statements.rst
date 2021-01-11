
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
     null | assignment | exit |
     goto | delay | raise |
     procedure_call | return |
     requeue | entry_call |
     abort | code
   
   compound_statement ::=
     if | case | loop |
     block | accept | select
 
----------------------------
Procedure Calls (Overview)
----------------------------

.. container:: columns

 .. container:: column
  
    * Details covered in "Subprograms" section
    * Procedure calls are statements so shown here
    * Traditional call notation is supported
    * "Distinguished Receiver" notation is supported

       - For `tagged` types only
       - Starting with Ada 2012

 .. container:: column
  
    .. code:: Ada
    
       procedure Activate (
          This : in out Foo;
          Wait : in Boolean);
       ...
       Idle : Foo;
       ...
       -- Traditional call
       Activate (Idle, True);
       -- "Distinguished Receiver"
       Idle.Activate ( True );
     
---------------------------------
Parameter Associations In Calls
---------------------------------

* Traditional "positional association" is allowed

   - Nth actual parameter goes to nth formal parameter

* "Named association" also allowed

   - Name of formal parameter is repeated

.. code:: Ada

   procedure Activate (This : in out Foo;
                       Wait : in Boolean);
   procedure Activate (This : in out Foo;
                       Wait : in Boolean);
   --
   Activate ( Idle, True ); -- positional
   Activate ( Idle, Wait => True ); -- named then positional
   Activate ( This => Idle, Wait => True ); -- named
   -- positional following named is a compile error
   Activate ( This => Idle, True );

=======================
Assignment Statements
=======================

----------
Examples
----------

.. include:: examples/040_statements/assignment_statements.rst

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

   type Bar is range 0 .. Max;
   type Foo is range -200 .. 200;
   ...
   F : Foo := 2; -- universal integer legal for any integer
   B : Bar := 2; -- universal integer legal for any integer
   F := B; -- compile error

----------------------------------------
Assignment Statements, Not Expressions
----------------------------------------

* Cannot be mixed within an expression, unlike expression-oriented languages (C, C++, etc.)

   - No Ada equivalent for these:

      .. code:: C++

         int a = b = c = 1;
         while (line = readline(file))
            { ...do something with line... }
 
* Prevents common coding error of unintentional assignment inside conditionals

   - E.g. `if ( a == 1 )` compared to `if ( a = 1 )`

      + Good coding practice says it should be written as `if ( 1 == a )`

------------------
Assignable Views
------------------

* Views control the way an entity can be treated

   - At different points in the program text

* The entity named must be an assignable variable

   - Thus the view of the target object must allow assignment

* Various situations constitute un-assignable views

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

* Prevent update to Target value

   - Target is not changed at all

* Are run-time errors, not illegalities

   - Predefined exception `Constraint_Error` is raised

* Can sometimes be detected by compiler

   - Static value
   - Value is outside base range of type

.. code:: Ada

   Max : Integer range 1 .. 100 := 100;
   ...
   Max := 0; -- run-time error
 
------------------------------------
Implicit Range Constraint Checking
------------------------------------

* Code like this

   .. code:: Ada

      procedure Demo is
        K : Integer;
        P : Integer range 0 .. 100;
      begin
        ...
        P := K;
        ...
      end Demo;
 
* Generates assignment check something like

   .. code:: Ada

      if K < 0 or K > 100 then
        raise Constraint_Error;
      else
        P := K;
      end if;
 
---------------------------------
Not All Assignments Are Checked
---------------------------------

* Compiler can assume variables of the same subtype have appropriate values
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

----------
Examples
----------

.. include:: examples/040_statements/conditional_statements.rst

--------------------
If-then Statements
--------------------

.. container:: columns

 .. container:: column
  
    * Control flow using Boolean expressions
    * Syntax
    
       .. code:: Ada
    
          if <boolean expression> then
             <statements>;
          end if;
     
       - Note parentheses are not required
       - At least one statement must be supplied

 .. container:: column
  
    .. code:: Ada
    
       if Valve(N) /= Closed then
         Isolate( Valve(N) );
         Notify( Valve_Failure,
                 Valve(N) );
       end if;

-------------------------
If-then-else Statements
-------------------------

.. container:: columns

 .. container:: column
  
    * Express exclusionary choice
    * Syntax
    
       .. code:: Ada
    
          if boolean_expression then
             sequence_of_statements;
          else
             sequence_of_statements;
          end if;
     
 .. container:: column
  
    .. code:: Ada
    
       declare
         Today, Next : Days;
       begin
         ...
         if Today = Days'Last then
           Next := Days'First;
         else
           Next := Days'Succ(Today);
         end if;
       end;
     
--------------------------
If-then-elsif Statements
--------------------------

.. container:: columns

 .. container:: column
  
    * Express exclusionary choice among several alternatives
    * Provide alternative to nesting
    * `elsif` indicates alternatives tested in textual order
    * `else` part is optional

 .. container:: column
    
    .. code:: Ada
    
       if A = 0 then
         Put_Line("Zero");
       elsif A < 10 then
         Put_Line("00" & A'image);
       elsif A < 100  then
         Put_Line("0" & A'image);
       -- as many "elsif" as needed
       else
         Put_Line(A'image);
       end if;

.. container:: speakernote

   Spelled that way on purpose, as was done in Python for example (differently, "elif")

----------------------------------------
Ugly Nested If-then Statements Example
----------------------------------------

.. code:: Ada

   if Direction = Forward then
      Go_Forward;
   else
      if Direction = Backward then
         Go_Backward;
      else
         if Direction = Left then
            Go_Left;
         else
            Go_Right;
         end if;
      end if;
   end if;
 
-----------------------------------------
If-then-elsif Statements Remove Nesting
-----------------------------------------

.. code:: Ada

   if Direction = Forward then
      Go_Forward;
   elsif Direction = Backward then
      Go_Backward;
   elsif Direction = Left then
      Go_Left;
   else
      Go_Right;
   end if;
 
-----------------
Case Statements
-----------------

* Express choice among equally-likely alternatives
* Syntax

   .. code:: Ada

      case <expression> is
        when <choice> => <statements>;
        { when <choice> => <statements>; }
      end case;

      choice ::= <expression> | <discrete_range> | others { "|" <other choice> }
 
* Expression is of an integer or enumeration type

-------------------------
Case Statements Example
-------------------------

.. code:: Ada

   type Directions is  (Forward, Backward, Left, Right);
   Direction : Directions;
   ...
   case Direction is
     when Forward => Go_Forward;
     when Backward => Go_Backward;
     when Left =>  Go_Left;
     when Right => Go_Right;
   end case;
   ...
 
----------------------
Case Statement Rules
----------------------

* All expression's possible values must be covered

   - Explicitly
   - Via `others`

* Choice values cannot be given more than once
* Choice values must be known at compile-time

------------------
 `Others` Choice
------------------

* Means "everything not specified so far"
* Thus must be last in the list of alternatives

.. code:: Ada
    
   case Today is   -- work schedule
     when Monday =>
       Arrive_Late;
       Leave_Early;
     when Tuesday | Wednesday | Thursday =>
       Arrive_Early;
       Leave_Late;
     when Friday =>
       Arrive_Early;
       Leave_Early;
     when others => -- weekend
       Stay_Home;
     end case;

------------------------------------
Case Statements Range Alternatives
------------------------------------

.. code:: Ada

   Week_Day : Days range Monday .. Friday := Today;
   ...
   case Week_Day is
     when Monday =>
       Arrive_Late;
       Leave_Early;
     when Tuesday .. Thursday =>
       Arrive_Early;
       Leave_Late;
     when Friday =>
       Arrive_Early;
       Leave_Early;
   end case;  
   ...
 
----------------------------------------
Why Not Use `Others` Case Alternative?
----------------------------------------

* Maintenance issue: suppose a new value is added that requires a new `case_statement_alternative`?
* If you forget the alternative, the compiler cannot detect that fact because the `others` covers the new value too!

.. code:: Ada
      
   type Agencies is (NASA, ESA, RFSA); -- could easily grow
   Bureau : Agencies;
   ...
   case Bureau is
     when ESA => ...
     when NASA => ...
     when others =>
       ... -- but this will handle growth without complaint
   end case;
   ...

=================
Loop Statements
=================

----------
Examples
----------

.. include:: examples/040_statements/loop_statements.rst

------------------------
Basic Loops and Syntax
------------------------

* Building blocks can express any kind of loop 

  - Basic loop
  - Optional iteration controls
  - Optional exit statements

* Syntax
    
   .. code:: Ada
    
     [<name> :] [iteration_scheme] loop
           <statements>
      end loop [<name>];
     
      iteration_scheme ::= while <boolean expression>
                           | for <loop_parameter_specification>
                           | for <loop_iterator_specification>
     
* Example

   .. code:: Ada
    
      loop
        Do_Something;
      end loop;
       
      Wash_Hair : loop
        Lather;
        Rinse;
      end loop Wash_Hair; -- "repeat"

.. container:: speakernote

    Loop Iterator Specification available in Ada2012 and later

-----------------
Exit Statements
-----------------

* Discontinue loop execution

   - Leaves innermost loop unless loop name is used

* Syntax
    
   .. code:: Ada
    
      exit [<loop name>] [when <boolean expression>];
     
   * `when` clause is shorthand for exit within an if-statement

.. code:: Ada
    
   loop
      ...
      if Time_to_Go then
        exit;
      end if;
      ...
    end loop;
       
    loop
      ...
      exit when Time_to_Go;
      ...
    end loop;
     
-------------------------
Exit Statement Examples
-------------------------

* Equivalent of C++ "do while" loop

   .. code:: Ada

      loop
        Do_Something;
        exit when Finished;
      end loop;
 
* Nested loops

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
     
   * Expression evaluated prior to execution of body
   * Loop will execute until expression is False

      - Using the language-defined type `Boolean`

* Example
  
   .. code:: Ada
    
      Count := 0;
      while Count < Largest loop
        Count := Count + 2;
        Display( List(Count) );
      end loop;
     
---------------------
For-loop Statements
---------------------

* Two high-level forms

   - Light-weight syntax
   - Focused on objects
   - Ideal when you want to visit every element of an object
   - Only available in Ada2012 and later

* One low-level form

   - Focused on explicitly specified sequences of values
   - Provides precise control over sequence

      + More syntax as a result

   - General-purpose (looping, array indexing, etc.)

* We focus on low-level form until section on arrays

-------------------------------
Low-Level For-loop Statements
-------------------------------

* Declares an object that takes on successive values of a discrete type
* How that object is used is entirely up to you

   - Not required to iterate over arrays or components
   - Can be used to access arrays by iterating over indexes

* Syntax

   .. code:: Ada

      for name in [reverse] discrete_subtype_definition loop
      ...
      end loop;
 
.. container:: speakernote

   Name - loop parameter object
   Discrete subtype definition - loop parameter type and range of values

-----------------------------------
Simple Low-Level For-loop Example
-----------------------------------

.. code:: Ada

   procedure Demo_LowLevel_ForLoops is
     type Days is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   begin
     for Today in Days range Mon .. Fri loop
       Put_Line( Days'Image(Today) );
     end loop;
     for K in Integer range -1 .. 10 loop
       Put_Line( Integer'Image(K) );
     end loop;
   end Demo_LowLevel_ForLoops;
 
-----------------------------------
Expressing the Sequence of Values
-----------------------------------

* Several syntactic forms allowed
* Just write what seems natural, it is likely legal

.. code:: Ada

   -- name without constraint
   for This_Day in Days loop
   -- name with constraint
   for This_Day in Days range Mon .. Fri
   -- named subtype
   for This_day in Weekdays loop
   -- expression .. Expression
   for This_day in Mon .. Fri loop
   -- variable(s) of type Days
   for This_day in Start .. Stop loop
 
-------------------------------
Low-Level For-loop Parameters
-------------------------------

* Declared implicitly by loop statement
* Has a constant view

   - No assignment
   - No update via subprogram parameter

* Step-size is always logically "one"

.. code:: Ada
    
   type Days is (Mon, Tues, Wed, Thurs, Fri, Sat, Sun);

   for Day in Days loop
      Days_IO.Put (Day);
      New_Line;
   end loop;
   
   for Day in Days range Mon .. Fri loop

   for Count in Leaf_Counter range 1 .. 10 loop

     
-----------------------------------
Low-Level For-loop Parameter Type
-----------------------------------

* Need not be specified explicitly

   - As long as can be deduced properly from range

   .. code:: Ada

      for Today in Mon .. Fri loop
 
* Predefined type `Integer` assumed for convenience

   - Each bound must be either a

      + Numeric literal, or
      + Named number, or
      + Attribute

   - And both of type `universal_integer`

-------------
Null Ranges
-------------

.. container:: columns

 .. container:: column
  
    * Occur when lower bound ``>`` upper bound

       - `1 .. 0`, `Bar .. Foo`, `Fri .. Mon`
       - Both literals and variables can specify null ranges

    * Cause no iteration at all (not even one)
    * Useful when calculating the upper bound

       - Otherwise pointless to code it explicitly!

 .. container:: column
  
    .. code:: Ada
    
       Get (File, Number_To_Read);
       -- if Number_To_Read is 0
       -- loop is not entered
       for K in 1 .. Number_To_Read
       loop
           Get (File, Work_Day);
       end loop;
     
-----------------------------------------
Reversing Low-Level Iteration Direction
-----------------------------------------

* Keyword `reverse` reverses iteration values

   .. code:: Ada

      for This_Day in reverse Mon .. Fri loop
 
* `Reverse` on a null range is still a null range

   - Thus no iteration

   .. code:: Ada

      for This_Day in reverse Fri .. Mon loop  -- no iterations
 
* Again not useful unless bounds not known until execution

   .. code:: Ada

      Get (First_Day, Second_Day);
      for This_Day in reverse First_Day .. Second_Day loop
 
---------------------------------------
For-Loop Parameter Visibility Pitfall
---------------------------------------

* Parameter obeys visibility rules as per any object
* Inner objects with same name hide outer objects

   .. code:: Ada

      declare
        Counter : Integer := 0;
        ...
      begin
        ...
        -- this COUNTER is not the integer COUNTER from above
        for Counter in Integer range A .. B loop
        ...
        end loop;
 
------------------------------------------
Combining Iterations and Exit Statements
------------------------------------------

.. container:: columns

 .. container:: column
  
    * Legal
    * Most reasonable with for-loops

       - The maximum number of iterations is specified but maybe an early exit can be made

 .. container:: column
  
    .. code:: Ada
    
       while Count < Largest loop
          ...
          Count := Count + 2;
          ...
          exit when Count > F(X);
          ...
       end loop;
       
       for K in 1 .. 1000 loop
          ...
          exit when Done_Processing;
          ...
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

==================
Block Statements
==================

------------------
Block Statements
------------------

.. container:: columns

 .. container:: column
  
    * Block statements are genuine statements

       - An optional declarative part is allowed
       - May be thought of as inline procedures

    * Useful when:

       - Declarations are only temporarily required
       - Declarations must be done as part of statement sequence
       - Exceptions must be trapped locally

 .. container:: column
  
    * Syntax
    
       .. code:: Ada
    
          [<name> :]
          [
          declare
            <declarative part>
          ]
          begin
             <statements>
          end [<name>];
     
--------------------------
Block Statements Example
--------------------------

.. code:: Ada

   begin
      Get (V);
      Get (U);
      if U > V then -- swap them
         Swap: -- name of the block
         declare
            Temp : Integer; -- only creates this when needed
         begin
            Temp := U;
            U := V;
            V := Temp;
         end Swap;
      end if;
      Print (U);
      Print (V);
   end;
 
--------------------------
Referencing Hidden Names
--------------------------

* Must copy for-loop parameter to some other object if needed after the loop exits
* Use dot notation with outer scope name when hiding occurs

.. code:: Ada

   Foo:
   declare
      Counter : Integer := 0;
   begin
      ...
      for Counter in Integer range 1 .. Number_Read loop
         -- set declared "Counter" to loop counter
         Foo.Counter := Counter;
         ...
      end loop;
      ...
   end Foo;

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

   - On occasion a `goto` is arguably the clearest approach

      .. code:: Ada

         <<Awaiting_Interrupt>>  goto Awaiting_Interrupt;
 
* Restrictions

   - Based on common sense
   - Example: cannot jump into a `case` statement

-------------------------------
Sample of Reasonable GOTO Use
-------------------------------

* Avoids complicated boolean expressions controlling whether, within a loop, to continue processing or to start the next iteration

.. code:: Ada

   while ... loop
      ...
      -- lots of code! 
      ...
      goto continue;      
      ...
      -- lots more code!
      ...
      ...
      <<continue>>
   end loop;

=================
Null Statements
=================

-----------------
Null Statements
-----------------

* Specify no action to be taken
* Used when a statement is required but no action intended
* Allows compiler to catch nasty oversights and editing accidents

.. code:: Ada

   case Today is
     when Monday .. Thursday =>
       Work (9.0);
     when Friday =>
       Work (4.0);
     when Saturday .. Sunday =>
       null;
   end case;

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
