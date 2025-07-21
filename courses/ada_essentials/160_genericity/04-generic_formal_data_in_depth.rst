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
   :number-lines: 3

   generic
      type Type_T is private;
      with function "*" (L, R : Type_T) return Type_T is <>;
   function Calculate (L, W : Type_T) return Type_T;


* :ada:`is <>`

   - If no subprogram specified for instance, compiler uses subprogram with **same**:

      - Name
      - Parameter profile (types only, not parameter name)

* Instantiations

   .. code:: Ada
      :number-lines: 4

      type Record_T is record
         Field : Integer;
      end record;
      function Multiply (L, R : Record_T) return Record_T; 
   
      function Allow_Default is new Calculate (Integer);
      function Specify_Operator is new Calculate (Record_T, Multiply);
      function Need_Operator is new Calculate (Record_T);

   * :ada:`Allow_Default` uses the implicit definition for :ada:`*`
   * :ada:`Specify_Operator` passes in the appropriate definition via :ada:`Multiply`
   * :ada:`Need_Operator` generates a compile error

      :color-red:`main.adb:11:4: error: instantiation error at gen.ads:5`

      :color-red:`gen.ads:5:1: error: instantiation error at gen.ads:5`

      :color-red:`main.adb:11:4: error: no visible subprogram matches the specification for "*"`

..
  language_version 2005

------------------------------------------------------
Generic Subprogram Parameters - Default Values (2/2)
------------------------------------------------------

.. code:: Ada
   :number-lines: 2

   procedure Toggle (Switch : in out Boolean);
   --  Toggle will flip Switch from True to False or
   --  False to True

   ---------------------------
   -- Definition of Generic --
   ---------------------------
   generic
     procedure Toggle (Switch : in out Boolean) is null;
   procedure Print (Switch : in out Boolean);

   -------------------------------
   -- Implementation of Generic --
   -------------------------------
   procedure Print (Switch : in out Boolean) is
   begin
      Toggle (Switch);
      Put_Line (Switch'Image);
   end Print;

* :ada:`is null` (for procedures only)

   - If no procedure is specified, a null procedure will be used

* :ada:`procedure Instance1 is new Print;`

   * Line 18 will call a null subprogram because generic formal parameter :ada:`Toggle`
     is not specified, so line 10 forces it to be a null subprogram

* :ada:`procedure Instance2 is new Print (Clean);`

   * Line 18 will call the implementation of the procedure defined on line 2, because
     that procedure is passed as the generic formal parameter :ada:`Toggle`

..
  language_version 2005
