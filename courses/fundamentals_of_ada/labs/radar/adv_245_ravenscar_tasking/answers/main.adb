with Ada.Text_IO;

with Devices.Radars;
with Radar_Internals;

procedure Main is
    E : Devices.Radars.Event_T;
begin
   while True loop
      Devices.Radars.Radar.Wait_Event (E);
      Ada.Text_IO.Put_Line
         ("event "
          & Radar_Internals.Object_Type_T'Image (E.Object)
          & " "
          & Radar_Internals.Object_Status_T'Image (E.Status) );
      Ada.Text_IO.Put_Line ("radar at angle " & Radar_Internals.Angle_Degrees_T'Image (Devices.Radars.Radar.Angle));
   end loop;
end Main;
