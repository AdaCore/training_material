with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C;
procedure Main is

   package Float_Io is new Ada.Text_IO.Float_IO (Interfaces.C.C_float);

   One_Minute_In_Seconds : constant := 60.0;
   One_Hour_In_Seconds   : constant := 60.0 * One_Minute_In_Seconds;

   type Distance_T is (Feet, Meters, Miles) with Convention => C;
   type Data_T is record
      Distance      : Interfaces.C.C_float;
      Distance_Type : Distance_T;
      Seconds       : Interfaces.C.C_float;
   end record with Convention => C_Pass_By_Copy;
   function C_Miles_Per_Hour (Data : Data_T) return Interfaces.C.C_float
      with Import, Convention => C, External_Name => "miles_per_hour";

   Object_Feet : constant Data_T :=
     (Distance => 6_000.0,
      Distance_Type => Feet,
      Seconds  => Interfaces.C.C_float(One_Minute_In_Seconds));
   Object_Meters : constant Data_T :=
     (Distance => 3_000.0,
      Distance_Type => Meters,
      Seconds  => Interfaces.C.C_float(One_Hour_In_Seconds));
   Object_Miles : constant Data_T :=
     (Distance => 1.0,
      Distance_Type =>
      Miles, Seconds => 1.0);

   procedure Run (Object : Data_T) is
   begin
      Float_Io.Put (Object.Distance);
      Put (" " & Distance_T'Image (Object.Distance_Type) & " in ");
      Float_Io.Put (Object.Seconds);
      Put (" seconds = ");
      Float_Io.Put (C_Miles_Per_Hour (Object));
      Put_Line (" mph");
   end Run;

begin
   Run (Object_Feet);
   Run (Object_Meters);
   Run (Object_Miles);
end Main;
