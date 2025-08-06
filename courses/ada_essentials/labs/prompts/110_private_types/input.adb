with Ada.Text_IO; use Ada.Text_IO;
package body Input is

   function Get return Colors.Color_Set_T is
      Ret_Val : Colors.Color_Set_T;
   begin
      -- Query user to determine what colors to add to the set
      return Ret_Val;
   end Get;

end Input;
