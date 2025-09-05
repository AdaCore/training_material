with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type Days_Of_Week_T is
     (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
   type Hours is mod 24;
   Start  : Hours;
   Finish : Hours;
begin
   -- Loop over Days_Of_Week_T

   --  Use a "case" statement to determine when the workday
   --  starts and ends based on the day of the week
   Start := 0;
   Finish := 23;

   --  Print a header indicating what day of the week it is
   Put_Line ("Day of Week");        --  (example)
   --  For Saturday, print a line for every other hour (e.g. 3, 5, 7)
   Put_Line (Hours'Image (Start));  --  (example)
   --  For other days, print a line for every hour
   Put_Line (Hours'Image (Finish)); --  (example)

end Main;
