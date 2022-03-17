with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   type Days_Of_Week_T is
     (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
   type Hours_Worked is digits 6;

   Total_Worked : Hours_Worked := 0.0;
   Hours_Today  : Hours_Worked;
   Overtime     : Hours_Worked;
begin
   -- For each day in the Days_Of_Week_T loop
      -- Print prompt indicating which day we're asking for
      -- Loop "forever"
         Hours_Today := Hours_Worked'value (Get_Line);
         -- exit the main loop if hours worked < 0
         if Hours_Today > 24.0 then
            Put_Line ("I don't believe you");
         else
            -- exit the input loop if a reasonable number is reached
            null;
         end if;
      -- if hours worked > 8 then
         Overtime    := Hours_Today - 8.0;
         Hours_Today := Hours_Today + 1.5 * Overtime;
      -- Calculate actual hours paid for based on day of the week
         -- Monday - Friday, just add hours worked
         -- Saturday - hours worked * 1.5
         -- Sunday - hours worked * 2

   Put_Line (Hours_Worked'image (Total_Worked));
end Main;
