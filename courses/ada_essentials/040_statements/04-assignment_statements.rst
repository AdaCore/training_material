=======================
Assignment Statements
=======================

-----------------------
Assignment Statements
-----------------------

* Syntax

   .. code:: Ada

      <variable> := <expression>;

* Value of expression is copied to target variable
* The type of the RHS must be same as the LHS

   - Rejected at compile-time otherwise

.. container:: latex_environment small

  .. code:: Ada

     declare
        type Miles_T is range 0 .. Max_Miles;
        type Km_T is range 0 .. Max_Kilometers;

        M : Miles_T := 2; -- universal integer legal for any integer
        K : Km_T := 2; -- universal integer legal for any integer
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

------------------
Assignable Views
------------------

* A :dfn:`view` controls the way an entity can be treated

   - At different points in the program text

* The named entity must be an assignable variable

   - Thus the view of the target object must allow assignment

* Various un-assignable views

   - Constants
   - Variables of :ada:`limited` types
   - Formal parameters of mode :ada:`in`

.. code:: Ada

   Max : constant Integer := 100;
   ...
   Max := 200; -- illegal

--------------------------------
Aliasing the Assignment Target
--------------------------------

.. admonition:: Language Variant

   Ada 2022

* C allows you to simplify assignments when the target is used in the expression. This avoids duplicating (possibly long) names.

   .. code:: C

      total = total + value;
      // becomes
      total += value;

* Ada 2022 implements this by using the target name symbol :ada:`@`

   .. code:: Ada

      Total := Total + Value;
      -- becomes
      Total := @ + Value;

* Benefit

   * Symbol can be used multiple times in expression

      .. code:: Ada

         Value := (if @ > 0 then @ else -(@));

* Limitation

   * Symbol is read-only (so it can't change during evaluation)

      .. code:: Ada

         function Update (X : in out Integer) return Integer;
         function Increment (X: Integer) return Integer;

      .. code:: Ada
         :number-lines: 13

            Value := Update (@);
            Value := Increment (@);

      ``example.adb:13:21: error: actual for "X" must be a variable``

------
Quiz
------

.. container:: latex_environment scriptsize

 .. container:: columns

  .. container:: column

   .. code:: Ada

      type One_T is range 0 .. 100;
      type Two_T is range 0 .. 100;
      A : constant := 100;
      B : constant One_T := 99;
      C : constant Two_T := 98;
      X : One_T := 0;
      Y : Two_T := 0;

  .. container:: column

   Which block(s) is (are) legal?

   A. | :answermono:`X := A;`
      | :answermono:`Y := A;`
   B. | :answermono:`X := B;`
      | :answermono:`Y := C;`
   C. | ``X := One_T(X + C);``
   D. | :answermono:`X := One_T(Y);`
      | :answermono:`Y := Two_T(X);`
   E. | ``B := One_T(Y) + X;``

   .. container:: animate

     Explanations

     A. Legal - :ada:`A` is an untyped constant so it can be used
        for any integer-based object
     B. Legal - :ada:`B, C` are correctly typed
     C. Illegal - No such "+" operator: must convert operand individually
     D. Legal - Correct conversion and types
     E. Illegal - Even though the right-hand side matches the type,
        :ada:`B` is a constant and cannot be modified
        

