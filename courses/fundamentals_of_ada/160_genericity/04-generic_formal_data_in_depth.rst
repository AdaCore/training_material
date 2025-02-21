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

Given the following generic function:

.. code:: Ada

   generic
      type Some_T is private;
      with function "+" (L : Some_T; R : Integer) return Some_T is <>;
   function Incr (Param : Some_T) return Some_T;

   function Incr (Param : Some_T) return Some_T is
   begin
      return Param + 1;
   end Incr;

And the following declarations:

.. code:: Ada

   type Record_T is record
      Component : Integer;
   end record;
   function Add (L : Record_T; I : Integer) return Record_T is
      ((Component => L.Component + I))
   function Weird (L : Integer; R : Integer) return Integer is (0);

Which of the following instantiation(s) is/are **not** legal?

A. ``function IncrA is new Incr (Integer, Weird);``
B. ``function IncrB is new Incr (Record_T, Add);``
C. :answermono:`function IncrC is new Incr (Record_T);`
D. ``function IncrD is new Incr (Integer);``

.. container:: animate

   :ada:`with function "+" (L : Some_T; R : Integer) return Some_T is <>;` indicates that if no function for :ada:`+` is passed in, find (if possible) a matching definition at the point of instantiation.

   A. :ada:`Weird` matches the subprogram profile, so :ada:`Incr` will use :ada:`Weird` when doing addition for :ada:`Integer`
   B. :ada:`Add` matches the subprogram profile, so :ada:`Incr` will use :ada:`Add` when doing the addition for :ada:`Record_T`
   C. There is no matching :ada:`+` operation for :ada:`Record_T`, so that instantiation fails to compile
   D. Because there is no parameter for the generic formal parameter :ada:`+`, the compiler will look for one in the scope of the instantiation. Because the instantiating type is numeric, the inherited :ada:`+` operator is found

..
  language_version 2005
