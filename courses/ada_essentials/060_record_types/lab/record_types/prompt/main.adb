with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   Max_Feet   : constant := 100;
   Max_Inches : constant := 12;

   --  Create separate types for feet and inches
   --  (Because adding inches to feet doesn't make sense)

   --  Distance_T should be a record with two fields
   type Distance_T is new Integer;

   Point_1  : Distance_T;
   Point_2  : Distance_T;
   Distance : Distance_T;

begin
   --  Set Point_1 to 12 feet 7 inches
   --  Set Point_2 to 6 feet 8 inches
   --  Add Point_1 and Point_2 together
   --     (Hint: result should be 19 feet 3 inches!)

   --  Print Point_1, Point_2, and Distance
   Put_Line ("Point1: ?");
   Put_Line ("Point2: ?");
   Put_Line ("Distance: ?");
end Main;
