==============
Introduction
==============

---------------------
Syntax and Examples
---------------------

* Syntax (simplified)

   .. code:: Ada

      type T is record
         Component_Name : Type [:= Default_Value];
         ...
      end record;

      type T_Empty is null record;

* Example

   .. code:: Ada

      type Record1_T is record
         Component1 : Integer;
         Component2 : Boolean;
      end record;

* Records can be **discriminated** as well

   .. code:: Ada

      type T (Size : Natural := 0) is record
         Text : String (1 .. Size);
      end record;

