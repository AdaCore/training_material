with Ada.Text_IO; use Ada.Text_IO;
with Sensors; use Sensors;
procedure Reader is
begin
   for I in 1 .. 10 loop
      Put_Line ( "Sensor: " & Sensor_T'image ( Sensors.Value ) );
      delay 0.5;
   end loop;
end Reader;
