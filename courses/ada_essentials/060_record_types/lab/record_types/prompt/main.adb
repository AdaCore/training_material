with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   Max_Feet   : constant := 100;
   Max_Inches : constant := 12;

   --  Create separate types for feet and inches
   --  (Because adding inches to feet doesn't make sense)

   --  Distance_T should be a record with two fields
   type Distance_T is new Integer;

   Ladder_Height : Distance_T;
   Worker_Height : Distance_T;
   Maximum_Reach : Distance_T;

begin
   --  Set Ladder_Height to 12 feet 7 inches
   --  Set Worker_Height to 6 feet 8 inches
   --  Add Ladder_Height and Worker_Height together
   --     (Hint: result should be 19 feet 3 inches!)

   --  Print Ladder_Height, Worker_Height, and Maximum_Reach
   Put_Line ("Ladder Height: ?");
   Put_Line ("Worker Height: ?");
   Put_Line ("Maximum Reach: ?");
end Main;
