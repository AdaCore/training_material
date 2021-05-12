with Ada.Text_IO; use Ada.Text_IO;
with Sensors; use Sensors;
procedure Test_Driver is
begin
   loop
      Put ( "X to quit, any other key to read the sensor" );
      declare
         Response : constant string := Get_Line;
      begin
         exit when Response'Length > 0 and then
            (Response(Response'first) = 'x' or
             Response(Response'first) = 'X');
         Put_Line ( "Sensor: " & Sensor_T'image ( Sensors.Value ) );
      end;
   end loop;
end Test_Driver;
