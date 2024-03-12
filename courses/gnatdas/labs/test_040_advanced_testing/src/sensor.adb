pragma Ada_2012;
package body Sensor is

   ----------
   -- Read --
   ----------

   function Read
     (Which : Sensor_T)
      return Integer is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      return raise Program_Error with "Unimplemented function Read";
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Which  :     Sensor_T;
      Value  :     Integer;
      Status : out Boolean) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

end Sensor;
