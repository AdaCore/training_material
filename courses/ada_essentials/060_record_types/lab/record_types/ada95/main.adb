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

   Ladder_Height : Distance_T;
   Worker_Height : Distance_T;
   Maximum_Reach : Distance_T;

   Total : Integer;
   --Declarations

   --Implementation
begin
   Ladder_Height.Feet   := 12;
   Ladder_Height.Inches := 7;

   Worker_Height := (Feet => 6, Inches => 8);

   Maximum_Reach := (0, 0);

   Total := Integer (Ladder_Height.Inches) + Integer (Worker_Height.Inches);
   if Total >= Max_Inches then
      Maximum_Reach.Inches := Inches_T (Total - Max_Inches);
      Maximum_Reach.Feet   := 1;
   else
      Maximum_Reach.Inches := Ladder_Height.Inches + Worker_Height.Inches;
   end if;
   Maximum_Reach.Feet :=
     Maximum_Reach.Feet + Ladder_Height.Feet + Worker_Height.Feet;

   Put_Line
     ("Ladder Height: " & Feet_T'Image (Ladder_Height.Feet) &
      Inches_T'Image (Ladder_Height.Inches));
   Put_Line
     ("Worker Height: " & Feet_T'Image (Worker_Height.Feet) &
      Inches_T'Image (Worker_Height.Inches));
   Put_Line
     ("Maximum Reach: " & Feet_T'Image (Maximum_Reach.Feet) &
      Inches_T'Image (Maximum_Reach.Inches));
end Main;
--Implementation
