   procedure P is
      type T1 is tagged null record;
      procedure Not_Prim (A : T1) is null;
      type T2 is new T1 with null record;

      O1 : T1;
      O2 : T2;
   begin
      -- Not a primitive
      Not_Prim (O1);
      -- Would not compile
      -- Not_Prim (O2);
   end P;
