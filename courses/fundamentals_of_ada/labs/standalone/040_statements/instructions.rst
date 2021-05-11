----------------
Statements Lab
----------------

* Requirements

   - Create a simple algorithm to count number of hours worked in a week

      + Use `Ada.Text_IO.Get_Line` to ask user for hours worked on each day
      + Any hours over 8 gets counted as 1.5 times number of hours (e.g. 10 hours worked will get counted as 11 hours towards total)
      + Saturday hours get counted at 1.5 times number of hours
      + Sunday hours get counted at 2 times number of hours

   - Print total number of hours "worked"

* Hints

   - Use `for` loop to iterate over days of week
   - Use `if` statement to determine overtime hours
   - Use `case` statement to determine weekend bonus

-----------------------------
Statements Lab Extra Credit
-----------------------------

* Use an inner loop when getting hours worked to check validity

   - Less than 0 should exit outer loop
   - More than 24 should not be allowed

-------------------------
Statements Lab Solution
-------------------------

.. code:: Ada

   Day_Loop :
   for Day in Days_Of_Week_T loop
     Put_Line (Day'Image);
     Input_Loop :
     loop
       Hours_Today := Hours_Worked'Value (Get_Line);
       exit Day_Loop when Hours_Today < 0.0;
       if Hours_Today > 24.0 then
         Put_Line ("I don't believe you");
       else
         exit Input_Loop;
       end if;
       end loop Input_Loop;

       if Hours_Today > 8.0 then
         declare
           Overtime : Hours_Worked := Hours_Today - 8.0;
         begin
           Hours_Today := Hours_Today + 1.5 * Overtime;
         end;
       end if;
       case Day is
         when Monday .. Friday =>
           Total_Worked := Total_Worked + Hours_Today;
         when Saturday =>
           Total_Worked := Total_Worked + Hours_Today * 1.5;
         when Sunday =>
           Total_Worked := Total_Worked + Hours_Today * 2.0;
       end case;
     end loop Day_Loop;

     Put_Line (Total_Worked'Image);
