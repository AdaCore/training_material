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

         V   : Positive := 10;
         Max : Float;

         procedure My_Repository is new Repository
           (Component_T    => Float,
            Array_size     => V,
            High_Watermark => Max);

------
Quiz
------

.. include:: ../quiz/genericity_type_and_variable/quiz.rst

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

------------------------------------------------------
Generic Subprogram Parameters - Default Values (1/2)
------------------------------------------------------

.. code:: Ada

   type Feet_T is digits 6;
   type Area_T is digits 6;
   function Times (L, R : Feet_T) return Area_T;

   generic
      with function "+" (L, R : Feet_T) return Feet_T is <>;
      with function "*" (L, R : Feet_T) return Area_T is <>;
   function Calculate (L1, L2 : Feet_T;
                       W1, W2 : Feet_T)
                       return Area_T;

* :ada:`is <>`

   - If no subprogram specified for instantiation, compiler will use a subprogram with:

      - Same name
      - Same parameter profile (types only, not parameter name)

* Legal instances:

   .. code:: Ada

      -- Explicit specifications for "plus" and "multiply"
      function Instance1 is new Calculate ("+", Times);
      -- Implicit specification for "plus", explicit for "multiply"
      function Instance2 is new Calculate ("*" => Times);

* Illegal instance

   .. code:: Ada

      -- There is no implicit function for "times"
      function Instance3 is new Calculate;

* Adding an implicit function for times would make :ada:`Instance3` legal

   .. code:: Ada

      function "*" (L, R : Feet_T) return Area_T;
      function Instance3 is new Calculate;

..
  language_version 2005

------------------------------------------------------
Generic Subprogram Parameters - Default Values (2/2)
------------------------------------------------------

.. code:: Ada

   type Miles_T is digits 6;
   procedure Clean (Miles : in out Miles_T) is
   begin
      Miles := (if Miles < 0.0 then 0.0 else Miles);
   end Clean;

   generic
     with procedure Clean (Miles : in out Miles_T) is null;
   procedure Print (Miles : in out Miles_T);

   procedure Print (Miles : in out Miles_T) is
   begin
      Clean (Miles);
      Put_Line (Miles'Image);
   end Print;

* :ada:`is null` (for procedures only)

   - If no procedure is specified, a null procedure will be used

*  Instances:

   .. code:: Ada

      Miles : Miles_T := -12.34;
      procedure Instance1 is new Print;
      procedure Instance2 is new Print (Clean);

* Result of running 

   * :ada:`Instance1 (Miles)` |rightarrow| **-12.34**
   * :ada:`Instance2 (Miles)` |rightarrow| **0.0**

..
  language_version 2005
