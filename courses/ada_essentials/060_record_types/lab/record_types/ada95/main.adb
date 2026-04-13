--Declarations
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   Max_Feet   : constant := 100;
   Max_Inches : constant := 12;

   type Feet_T is range 0 .. Max_Feet;
   type Inches_T is range 0 .. Max_Inches - 1;

   type Distance_T is record
      Feet   : Feet_T;
      Inches : Inches_T;
   end record;

   Point_1  : Distance_T;
   Point_2  : Distance_T;
   Distance : Distance_T;

   Total : Integer;
   --Declarations

--Implementation
begin
   Point_1.Feet   := 12;
   Point_1.Inches := 7;

   Point_2 := (Feet   => 6,
               Inches => 8);

   Distance := (0, 0);

   Total := Integer (Point_1.Inches) +
            Integer (Point_2.Inches);
   if Total >= Max_Inches then
      Distance.Inches := Inches_T (Total - Max_Inches);
      Distance.Feet   := 1;
   else
      Distance.Inches := Point_1.Inches + Point_2.Inches;
   end if;
   Distance.Feet := Distance.Feet + Point_1.Feet + Point_2.Feet;

   Put_Line ("Point 1: " &
               Feet_T'Image(Point_1.Feet) &
               Inches_T'Image(Point_1.Inches));
   Put_Line ("Point 2: " &
               Feet_T'Image(Point_2.Feet) &
               Inches_T'Image(Point_2.Inches));
   Put_Line ("Distance: " &
               Feet_T'Image(Distance.Feet) &
               Inches_T'Image(Distance.Inches));
end Main;
--Implementation
