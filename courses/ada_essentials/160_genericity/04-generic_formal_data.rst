=====================
Generic Formal Data
=====================

--------------------------------------------
Generic Constants and Variables Parameters
--------------------------------------------

.. container:: columns

 .. container:: column

    * Variables can be specified on the generic contract
    * The mode specifies the way the variable can be used:

       - :ada:`in` |rightarrow| read only
       - :ada:`in out` |rightarrow| read write

    * Generic variables can be defined after generic types

 .. container:: column

    .. code:: Ada

       generic
          type T is private;
          X1 : Integer;  -- constant
          X2 : in out T; -- variable
       procedure P;

       V : Float;

       procedure P_I is new P
          (T  => Float,
           X1 => 42,
           X2 => V);

-------------------------------
Generic Subprogram Parameters
-------------------------------

* Subprograms can be defined in the generic contract
* Must be introduced by :ada:`with` to differ from the generic unit

   .. code:: Ada

      generic
         with procedure Callback;
      procedure P;
      procedure P is
      begin
         Callback;
      end P;
      procedure Something is null;
      procedure P_I is new P (Something);

------
Quiz
------

.. include:: ../quiz/genericity_type_and_variable/quiz.rst

------
Quiz
------

.. include:: ../quiz/genericity_limited_type/quiz.rst

