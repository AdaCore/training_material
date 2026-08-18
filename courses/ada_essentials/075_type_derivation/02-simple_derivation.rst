===================
Simple Derivation
===================

------------------------
Simple Type Derivation
------------------------

* Most types can be derived

  .. code:: Ada
    :font-size: footnotesize

    type Natural_T is new Integer_T range 0 .. Integer_T'Last;

* :ada:`Natural_T` inherits from:

   - The data **representation** of the parent

      * Integer based, 64 bits

   - The **primitives** of the parent

      * :ada:`Increment_With_Truncation` and :ada:`Increment_With_Rounding`

* The types are not the same

  .. code:: Ada
      :number-lines:  1

    procedure Main is
      type Integer_T is range 0 .. 100;
      type Natural_T is new Integer_T;
      I_Obj : Integer_T := 0;
      N_Obj : Natural_T := 0;
   begin
      I_Obj := N_Obj;
      I_Obj := Integer_T (N_Obj);
   end Main;

  * Assigning one to the other generates a compile error

    .. code:: error
      :font-size: tiny
    
      main.adb:7:16: error: expected type "Integer_T" defined at line 2
      main.adb:7:16: error: found type "Natural_T" defined at line 3

  * But a child can be converted to the parent (line 8)

--------------------------------------
Simple Derivation and Type Structure
--------------------------------------

* The type "structure" can not change

   - :ada:`array` cannot become :ada:`record`
   - Integers cannot become floats

* But can be **constrained** further
* Scalar ranges can be reduced

  .. code:: Ada
    :font-size: footnotesize

     type Positive_T is new Natural_T range 1 .. Natural_T'Last;

* Unconstrained types can be constrained

  .. code:: Ada
    :font-size: small

     type Arr_T is array (Integer range <>) of Integer;
     type Ten_Elem_Arr_T is new Arr_T (1 .. 10);
     type Rec_T (Size : Integer) is record
        Elem : Arr_T (1 .. Size);
     end record;
     type Ten_Elem_Rec_T is new Rec_T (10);

