with Interfaces;
with System.Storage_Elements;
package body Odometer is

   use type Base_Types.Meters_T;

   Sensor : Interfaces.Unsigned_32 with
      Volatile,
      Address => System.Storage_Elements.To_Address (16#1111_2222#);
   --  LSB = 1 mm

   function Read return Base_Types.Meters_T is
      Current : Interfaces.Unsigned_32 := Sensor;
   begin
      return Base_Types.Meters_T (Current) / 1_000.0;
   end Read;

end Odometer;
