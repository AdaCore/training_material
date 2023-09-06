with Ada.Text_IO;
use Ada.Text_IO;
with Interfaces.C;

procedure Main is

   package Float_Io is new Ada.Text_IO.Float_IO (Interfaces.C.C_float);

   type Data_T is null record; -- complete this
   Object_Feet : Data_T;
   Object_Meters : Data_T;
   Object_Miles : Data_T;

   procedure Run (Object : Data_T) is
   begin
      null;
      -- Print object data
      -- Call C function to calculate speed
      -- Print speed
   end Run;

begin

   Run (Object_Feet);
   Run (Object_Meters);
   Run (Object_Miles);

end Main;
