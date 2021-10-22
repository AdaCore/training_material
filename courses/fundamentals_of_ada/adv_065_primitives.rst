*********************
Advanced Primitives
*********************

=================
Type Derivation
=================

------------------------------
Primitive of Multiple Types
------------------------------

* A subprogram can be a primitive of several types

      .. code:: Ada

         package P is
            type T1 is range 1 .. 10;
            type T2 is (A, B, C);

            procedure Proc (V1 : T1; V2 : T2);
            function "+" (V1 : T1; V2 : T2) return T1;
         end P;

--------------
Freeze Point
--------------

* Ada doesn't explicitly identify the end of members declaration
* This end is the implicit **freeze point** occurring whenever:

   - A **variable** of the type is **declared**
   - The type is **derived**
   - The **end of the scope** is reached

* Subprograms past this point are not primitive

.. code:: Ada

   type Root is Integer;
   procedure Prim (V : Root);
   type Child is new Root; -- freeze root
   procedure Prim2 (V : Root); -- Not a primitive

   V : Child; --  freeze child
   procedure Prim3 (V : Child); -- Not a primitive

-------------------------------
Implicit Primitive Operations
-------------------------------

* Type declaration implicitly creates primitives

    - Numerical and logical operations
    - Code can overload or remove them

   .. code:: Ada

      package P is
         type T1 is range 1 .. 10;
         -- implicit: function "+" (Left, Right : T1) return T1;
      end P;
      ...
      procedure Main is
         V1, V2 : T1;
      begin
         V1 := V1 + V2;
      end Main;
