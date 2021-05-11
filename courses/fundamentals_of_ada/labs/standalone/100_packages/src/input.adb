
with Ada.Text_IO; use Ada.Text_IO;
with Constants;
package body Input is

   function Get_Value
     (Prompt : String)
      return Integer is
      Ret_Val : Integer;
   begin
      Put (Prompt & "> ");
      loop

         Ret_Val := Integer'Value (Get_Line);
         exit when Ret_Val >= Constants.Lowest_Value
           and then Ret_Val <= Constants.Highest_Value;
         Put ("Invalid. Try Again >");
      end loop;
      return Ret_Val;

   end Get_Value;

end Input;
