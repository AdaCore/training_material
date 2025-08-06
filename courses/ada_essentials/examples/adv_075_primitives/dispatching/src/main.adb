procedure Main is
   package Sub is
      --$ begin cut
      type T is tagged null record;

      procedure Prim (A : T) is null;
      procedure Not_Prim (A : T'Class) is null;
      --$ end cut
   end Sub;
   use Sub;
begin
   --$ begin cut
   declare
       type T2 is new T with null record;
       A : T'Class := T2'(null record);
   begin
       Prim (A);
       Not_Prim (A);
   end;
   --$ end cut
end Main;
