
with Ada.Text_IO; use Ada.Text_IO;
package body Input is

   function Get return Colors.Color_Set_T is
      Ret_Val : Colors.Color_Set_T;
   begin
      Put ("Enter color(s) ( ");
      for C in Colors.Color_T
      loop
         Put (Colors.Color_T'Image (C) & " ");
      end loop;
      Put_Line ("): ");
      loop
         declare
            Str : constant String := Get_Line;
         begin
            exit when Str'Length = 0;
            Colors.Add (Ret_Val, Colors.Color_T'Value (Str));
         end;
      end loop;
      return Ret_Val;
   end Get;

end Input;
