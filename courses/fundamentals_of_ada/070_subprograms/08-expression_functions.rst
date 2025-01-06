======================
Expression Functions
======================

----------------------
Expression Functions
----------------------

* Functions whose implementations are pure expressions

   - No other completion is allowed
   - No :ada:`return` keyword

* May exist only for sake of pre/postconditions

.. code:: Ada

   function function_specification is (expression);

NB: Parentheses around expression are **required**

* Can complete a prior declaration

.. code:: Ada

   function Squared (X : Integer) return Integer;
   function Squared (X : Integer) return Integer is
      (X ** 2);

..
  language_version 2012

------------------------------
Expression Functions Example
------------------------------

* Expression function

.. code:: Ada

   function Square (X : Integer) return Integer is (X ** 2);

* Is equivalent to

.. code:: Ada

   function Square (X : Integer) return Integer is
   begin
      return X ** 2;
   end Square;

..
  language_version 2012

------
Quiz
------

Which statement is True?

   A. Expression functions cannot be nested functions.
   B. Expression functions require a specification and a body.
   C. Expression functions must have at least one :ada:`return` statement.
   D. :answer:`Expression functions can have "out" parameters.`

.. container:: animate

   Explanation

   A. They **can** be nested subprograms (just like any other subprogram)
   B. As in other subprograms, the implementation can serve as the specification
   C. Because they are expressions, the :ada:`return` statement is not allowed
   D. An expression function does not allow assignment statements, but it can call another function that is **not** an expression function.

      .. code:: Ada

         function Normal_Fun (Input  :     Character;
                              Output : out Integer)
                              return Boolean is
         begin
            Output := Character'Pos (Input);
            return True;
         end Normal_Fun;

         function Expr_Fun (Input  :     Character;
                            Output : out Integer)
                            return Boolean is
            (Normal_Fun (Character'Succ (Input), Output));
