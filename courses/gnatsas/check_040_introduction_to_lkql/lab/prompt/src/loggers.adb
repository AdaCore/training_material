pragma Ada_2012;
package body Loggers is

   ------------------------
   -- Severity_To_String --
   ------------------------

   function Severity_To_String
     (Level : Severity_Level)
      return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Severity_To_String unimplemented");
      return
        raise Program_Error with "Unimplemented function Severity_To_String";
   end Severity_To_String;

   -----------------
   -- Log_Message --
   -----------------

   overriding procedure Log_Message
     (Logger  : in out Console_Logger;
      Level   :        String;
      Message :        String) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Log_Message unimplemented");
      raise Program_Error with "Unimplemented procedure Log_Message";
   end Log_Message;

   -------------------------
   -- Enable_Color_Output --
   -------------------------

   procedure Enable_Color_Output
     (Logger : in out Console_Logger;
      Enable :        Boolean) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Enable_Color_Output unimplemented");
      raise Program_Error with "Unimplemented procedure Enable_Color_Output";
   end Enable_Color_Output;

   ----------------
   -- Should_Log --
   ----------------

   overriding function Should_Log
     (Filter : Minimum_Severity_Filter;
      Level  : String)
      return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Should_Log unimplemented");
      return raise Program_Error with "Unimplemented function Should_Log";
   end Should_Log;

   -------------------
   -- Create_Filter --
   -------------------

   function Create_Filter
     (Min_Level : Severity_Level)
      return Minimum_Severity_Filter is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Create_Filter unimplemented");
      return raise Program_Error with "Unimplemented function Create_Filter";
   end Create_Filter;

end Loggers;
