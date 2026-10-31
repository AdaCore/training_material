=======================
Assignment Statements
=======================

-----------------------
Assignment Statements
-----------------------

**Syntax**

.. code:: bnf

  assignment_statement ::= 
     variable_name := expression;

* Value of expression is copied to target variable
* The type of the RHS must be same as the LHS

   - Rejected at compile-time otherwise

.. code:: Ada

  declare
     type Miles_T is range 0 .. Max_Miles;
     type Km_T is range 0 .. Max_Kilometers;

     M : Miles_T := 2;
     K : Km_T := 2;
  begin
     M := K; -- compile error

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

   - E.g. :ada:`if (a == 1)` compared to :ada:`if (a = 1)`
