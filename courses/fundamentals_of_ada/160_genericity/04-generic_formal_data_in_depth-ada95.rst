=====================
Generic Formal Data
=====================

--------------------------------------------
Generic Constants/Variables As Parameters
--------------------------------------------

.. container:: columns

 .. container:: column

    * Variables can be specified on the generic contract
    * The mode specifies the way the variable can be used:

       - :ada:`in` |rightarrow| read only
       - :ada:`in out` |rightarrow| read write

    * Generic variables can be defined after generic types

 .. container:: column

   .. container:: latex_environment tiny

     * Generic package

       .. code:: Ada

          generic
            type Component_T is private;
            Array_Size     : Positive;
            High_Watermark : in out Component_T;
          package Repository is

     * Generic instance

       .. code:: Ada

         V   : Float;
         Max : Float;

         procedure My_Repository is new Repository
           (Component_T    => Float,
            Array_size     => 10,
            High_Watermark => Max);

-------------------------------
Generic Subprogram Parameters
-------------------------------

* Subprograms can be defined in the generic contract
* Must be introduced by :ada:`with` to differ from the generic unit

   .. code:: Ada

      generic
         type T is private;
         with function Less_Than (L, R : T) return Boolean;
      function Max (L, R : T) return T;

      function Max (L, R : T) return T is
      begin
         if Less_Than (L, R) then
            return R;
         else
            return L;
         end if;
      end Max;

      type Something_T is null record;
      function Less_Than (L, R : Something_T) return Boolean;
      procedure My_Max is new Max (Something_T, Less_Than);

------
Quiz
------

.. include:: ../quiz/genericity_type_and_variable/quiz.rst
