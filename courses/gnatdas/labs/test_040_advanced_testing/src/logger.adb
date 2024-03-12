pragma Ada_2012;
package body Logger is

   ---------------
   -- Log_Error --
   ---------------

   procedure Log_Error (Message : String) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Log_Error unimplemented");
      raise Program_Error with "Unimplemented procedure Log_Error";
   end Log_Error;

end Logger;
