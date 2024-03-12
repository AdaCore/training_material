with Logger;
package body Simple is
   procedure Read
     (Which  :        Sensor.Sensor_T;
      Value  : in out Integer;
      Status :    out Boolean) is
   begin
      Value  := Sensor.Read (Which);
      Status := True;
      case Which is
         when Sensor.Speed =>
            if Value < 0 or Value > 99 then
               Status := False;
               Logger.Log_Error ("Invalid Speed");
            end if;
         when Sensor.Heading =>
            if Value < 0 or Value > 359 then
               Status := False;
               Logger.Log_Error ("Invalid Heading");
            end if;
         when Sensor.Altitude =>
            if Value < -100 or Value > 100_000 then
               Status := False;
               Logger.Log_Error ("Invalid Altitude");
            end if;
      end case;
   end Read;

   procedure Write
     (Which  :     Sensor.Sensor_T;
      Value  :     Integer;
      Status : out Boolean) is
   begin
      Sensor.Write (Which, Value, Status);
      if not Status then
         Logger.Log_Error ("Write " & Which'Image & " failed");
      end if;
   end Write;
end Simple;
