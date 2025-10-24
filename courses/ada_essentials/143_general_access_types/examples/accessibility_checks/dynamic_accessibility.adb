with Ada.Text_IO; use Ada.Text_IO;
procedure Dynamic_Accessibility is
   --|snippet_begin
   type Gen_Access_T is access all Integer;
   Global_Access : Gen_Access_T;
   Global_Object : aliased Integer := 123;
   procedure Set_Value (Param : access Integer) is
   begin
      Put_Line (Param.all'Image);
      Global_Access := Gen_Access_T (Param);
   end Set_Value;
begin
   Set_Value (Global_Object'Access);
   declare
      Local_Object : aliased Integer := 987;
   begin
      Set_Value (Local_Object'Access);
   end;
   --|snippet_end
end Dynamic_Accessibility;
