=================
Loop Statements
=================

------------------------
Basic Loops and Syntax
------------------------

**Syntax**

.. code:: bnf

  loop_statement ::= 
     [loop_statement_identifier:]
        [iteration_scheme] loop
           sequence_of_statements
         end loop [loop_identifier];
  
  iteration_scheme ::= while condition
     | for loop_parameter_specification
     | for iterator_specification
  
  loop_parameter_specification ::= 
     defining_identifier in [reverse] discrete_subtype_definition

* All kind of loops can be expressed

  - Optional iteration controls
  - Optional exit statements

**Example**

.. code:: Ada

     Wash_Hair : loop
       Lather (Hair);
       Rinse (Hair);
     end loop Wash_Hair;

-----------------------
While-loop Statements
-----------------------

**Syntax**

.. code:: bnf

  while condition loop
     sequence_of_statements
  end loop;

* Behaves the same as :ada:`loop` with :ada:`exit` at the beginning

  .. code:: bnf

     loop
        exit when not <boolean_expression>;
        sequence_of_statements
     end loop;

**Example**

.. code:: Ada

  while Count < Largest loop
     Count := Count + 2;
     Display (Count);
  end loop;

---------------------
"for in" Statements
---------------------

**Syntax**

.. code:: bnf

  for identifier in [reverse] discrete_subtype_definition
  loop
     sequence_of_statements
  end loop;

* Successive values of a **discrete** type

   - eg. enumerations values

**Example**

.. code:: Ada

  for Day in Days_T loop
     Refresh_Planning (Day);
  end loop;
