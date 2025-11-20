package body Nesting_Example is
   type Access_Level_0 is access all Integer;
   Pointer_At_0 : Access_Level_0;
   Object_At_0  : aliased Integer;

   procedure Proc is
      type Access_Level_1 is access all Integer;
      Pointer_At_1 : Access_Level_1;
      Object_At_1  : aliased Integer;
   begin
      Pointer_At_0 := Object_At_0'Access;
      Pointer_at_0 := Object_at_1'Access; -- illegal
      Pointer_At_1 := Object_At_0'Access;
      Pointer_At_1 := Object_At_1'Access;
      Pointer_At_1 := Access_Level_1 (Pointer_At_0);
      Pointer_At_1 := new Integer;
      Pointer_at_0 := Access_Level_0 (Pointer_at_1); -- illegal
   end Proc;
end Nesting_Example;
