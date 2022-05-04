procedure Main is

   --$ begin cut
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
   --$ end cut

   --  Works for non-tagged types
   type T_Int is new Integer;
   procedure Prim (A : T_Int) is null;
   type T2_Int is new T_Int;

   A : T2_Int;
begin
   Prim (A);
end Main;
