.. code:: Ada

   procedure P is
      type T is tagged null record;
      procedure Not_Prim (A : T) is null;
      type T2 is tagged null record;

      A : T;
      B : T2;
   begin
      -- Not a primitive
      Not_Prim (A);
      -- Would not compile
      -- Not_Prim (B);
   end P;
