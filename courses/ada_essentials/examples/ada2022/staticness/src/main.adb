procedure Main is
   --$ begin cut
   subtype T is Integer range 0 .. 2;
   function In_T (A : Integer)
      return Boolean is
      (A in T) with Static;
   --$ end cut
begin
   null;
end Main;
