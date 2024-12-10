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
            type Element_T is private;
            Array_Size     : Positive;
            High_Watermark : in out Element_T;
          package Repository is

     * Generic instance

       .. code:: Ada

         V   : Float;
         Max : Float;

         procedure My_Repository is new Repository
           (Element_T      => Float,
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

----------------------------------------
Generic Subprogram Parameters Defaults
----------------------------------------

* :ada:`is <>` - matching subprogram is taken by default
* :ada:`is null` - null procedure is taken by default

   - Only available in Ada 2005 and later

   .. code:: Ada

      generic
        type T is private;
        with function Is_Valid (P : T) return Boolean is <>;
        with procedure Error_Message (P : T) is null;
      procedure Validate (P : T);

      function Is_Valid_Record (P : Record_T) return Boolean;

      procedure My_Validate is new Validate (Record_T,
                                             Is_Valid_Record);
      -- Is_Valid maps to Is_Valid_Record
      -- Error_Message maps to a null procedure

..
  language_version 2005

------
Quiz
------

.. include:: ../quiz/genericity_type_and_variable/quiz.rst

------
Quiz
------

.. container:: columns

 .. container:: column


   .. code:: Ada
      :number-lines: 1

      procedure Double (X : in out Integer);
      procedure Square (X : in out Integer);
      procedure Half (X : in out Integer);
      generic
         with procedure Double (X : in out Integer) is <>;
         with procedure Square (X : in out Integer) is null;
      procedure Math (P : in out Integer);
      procedure Math (P : in out Integer) is
      begin
         Double (P);
         Square (P);
      end Math;
      procedure Instance is new Math (Double => Half);
      Number : Integer := 10;

 .. container:: column

 .. container:: column

   What is the value of Number after calling :ada:`Instance (Number)`

   A. 20
   B. 400
   C. :answer:`5`
   D. 10

.. container:: animate

  A. Would be correct for :ada:`procedure Instance is new Math;`

  B. Would be correct for either :ada:`procedure Instance is new Math (Double, Square);` *or* :ada:`procedure Instance is new Math (Square => Square);`

  C. Correct

    * We call formal parameter :ada:`Double`, which has been assigned to actual subprogram :ada:`Half`, so :ada:`P`, which is 10, is halved.

    * Then we call formal parameter :ada:`Square`, which has no actual subprogram, so it defaults to :ada:`null`, so nothing happens to :ada:`P`

  D. Would be correct for either :ada:`procedure Instance is new Math (Double, Half);` *or* :ada:`procedure Instance is new Math (Square => Half);`

..
  language_version 2005

----------------------
Quiz Answer in Depth
----------------------

      A. Wrong - result for :ada:`procedure Instance is new Math;`
      B. Wrong - result for :ada:`procedure Instance is new Math (Double, Square);`
      C. :ada:`Double` at line 10 is mapped to :ada:`Half` at line 3, and :ada:`Square` at line 11 wasn't specified so it defaults to :ada:`null`
      D. Wrong - result for :ada:`procedure Instance is new Math (Square => Half);`

.. container:: animate

  .. container:: latex_environment tiny

    :ada:`Math` is going to call two subprograms in order, :ada:`Double` and :ada:`Square`, but both of those come from the formal data.

    Whatever is used for :ada:`Double`, will be called by the :ada:`Math` instance. If nothing is passed in, the compiler tries to find a subprogram named :ada:`Double` and use that. If it doesn't, that's a compile error.

    Whatever is used for :ada:`Square`, will be called by the :ada:`Math` instance. If nothing is passed in, the compiler will treat this as a null call.

    In our case, :ada:`Half` is passed in for the first subprogram, but nothing is passed in for the second, so that call will just be null.

    So the final answer should be 5 (hence letter C).

