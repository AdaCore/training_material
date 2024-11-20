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

     [<name> :] [iteration_scheme] loop
           <statements>
      end loop [<name>];

   .. code::

      iteration_scheme ::= while <boolean expression>
                           | for <loop_parameter_specification>
                           | for <loop_iterator_specification>

* Example

   .. code:: Ada

      Wash_Hair : loop
        Lather (Hair);
        Rinse (Hair);
      end loop Wash_Hair;

--------------------
Loop Exit Statements
--------------------

* Leaves innermost loop

   - Unless loop name is specified

* Syntax

   .. code:: Ada

      exit [<loop name>] [when <boolean expression>];

* :ada:`exit when` exits with condition

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

* Equivalent to C's :C:`do while`

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

* One low-level form

   - General-purpose (looping, array indexing, etc.)
   - Explicitly specified sequences of values
   - Precise control over sequence

* Two high-level forms

   - Focused on objects
   - Seen later with Arrays

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

-----------------------------------
Variable and Sequence of Values
-----------------------------------

* Variable declared implicitly by loop statement

   - Has a view as constant
   - No assignment or update possible

* Initialized as :ada:`'First`, incremented as :ada:`'Succ`
* Syntactic sugar: several forms allowed

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

* The type can be implicit

   - As long as it is clear for the compiler
   - Warning: same name can belong to several enums

.. container:: latex_environment scriptsize

    .. code:: Ada
      :number-lines: 1

      procedure Main is
         type Color_T is (Red, White, Blue);
         type Rgb_T is (Red, Green, Blue);
      begin
         for Color in Red .. Blue loop  -- which Red and Blue?
            null;
         end loop;
         for Color in Rgb_T'(Red) .. Blue loop -- OK
            null;
         end loop;

    ::

      main.adb:5:21: error: ambiguous bounds in range of iteration
      main.adb:5:21: error: possible interpretations:
      main.adb:5:21: error: type "Rgb_T" defined at line 3
      main.adb:5:21: error: type "Color_T" defined at line 2
      main.adb:5:21: error: ambiguous bounds in discrete range

* If bounds are `universal_integer`, then type is :ada:`Integer` unless otherwise specified

   .. code:: Ada

      for Idx in 1 .. 3 loop -- Idx is Integer

      for Idx in Short range 1 .. 3 loop -- Idx is Short

-------------
Null Ranges
-------------

    * :dfn:`Null range` when lower bound ``>`` upper bound

       - :ada:`1 .. 0`, :ada:`Fri .. Mon`
       - Literals and variables can specify null ranges

    * No iteration at all (not even one)
    * Shortcut for upper bound validation

    .. code:: Ada

      -- Null range: loop not entered
      for Today in Fri .. Mon loop

-----------------------------------------
Reversing Low-Level Iteration Direction
-----------------------------------------

* Keyword :ada:`reverse` reverses iteration values

    - Range must still be ascending
    - Null range still cause no iteration

   .. code:: Ada

      for This_Day in reverse Mon .. Fri loop

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

--------------------------
Referencing Hidden Names
--------------------------

* Must copy for-loop parameter to some other object if needed after the loop exits
* Use dot notation with outer scope name when hiding occurs

.. code:: Ada

   Foo:
   declare
      Counter : Float := 0.0;
   begin
      ...
      for Counter in Integer range 1 .. Number_Read loop
         -- set declared "Counter" to loop counter
         Foo.Counter := Float (Counter);
         ...
      end loop;
      ...
   end Foo;

--------------------------
Iterations Exit Statements
--------------------------

* Early loop exit
* Syntax

  .. code:: Ada

        exit [<loop_name>] [when <condition>]

* No name: Loop exited **entirely**

    - Not only current iteration

  .. code:: ada

     for K in 1 .. 1000 loop
        exit when K > F(K);
     end loop;

* With name: Specified loop exited

  .. code:: ada

     for J in 1 .. 1000 loop
         Inner: for K in 1 .. 1000 loop
            exit Inner when K > F(K);
         end loop;
     end loop;

--------------------------------------
For-Loop with Exit Statement Example
--------------------------------------

.. code:: Ada

   -- find position of Key within Table
   Found := False;
   -- iterate over Table
   Search : for Index in Table'Range loop
     if Table (Index) = Key then
       Found := True;
       Position := Index;
       exit Search;
     elsif Table (Index) > Key then
       -- no point in continuing
       exit Search;
     end if;
   end loop Search;

.. container:: speakernote

   We use the low-level for-loop form because we want to capture the actual position of the key within the table.

------
Quiz
------

.. code:: Ada

   A, B : Integer := 123;

Which loop block(s) is (are) legal?

  A. | ``for A in 1 .. 10 loop``
     |    ``A := A + 1;``
     | ``end loop;``
  B. | :answermono:`for B in 1 .. 10 loop`
     |    :answermono:`Put_Line (Integer'Image (B));`
     | :answermono:`end loop;`
  C. | :answermono:`for C in reverse 1 .. 10 loop`
     |    :answermono:`Put_Line (Integer'Image (C));`
     | :answermono:`end loop;`
  D. | :answermono:`for D in 10 .. 1 loop`
     |    :answermono:`Put_Line (Integer'Image (D));`
     | :answermono:`end loop;`

.. container:: animate

   Explanations

   A. Cannot assign to a loop parameter
   B. Legal - 10 iterations
   C. Legal - 10 iterations
   D. Legal - 0 iterations

.

