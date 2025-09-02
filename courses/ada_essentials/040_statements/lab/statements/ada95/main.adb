with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   type Days_Of_Week_T is
     (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
<<<<<<< HEAD
   type Hours_Worked is digits 6;

   Total_Worked : Hours_Worked := 0.0;
   Hours_Today  : Hours_Worked;
   Overtime     : Hours_Worked;
   S            : String(1..10);
   L            : Integer;
begin
   Day_Loop :
   for Day in Days_Of_Week_T loop
      Put_Line (Days_Of_Week_T'Image (Day));
      Input_Loop :
      loop
         Get_Line (S, L);
         Hours_Today := Hours_Worked'Value (S(1..L));
         exit Day_Loop when Hours_Today < 0.0;
         if Hours_Today > 24.0 then
            Put_Line ("I don't believe you");
         else
            exit Input_Loop;
         end if;
      end loop Input_Loop;
      if Hours_Today > 8.0 then
         Overtime := Hours_Today - 8.0;
         Hours_Today := Hours_Today + 1.5 * Overtime;
      end if;
      case Day is
         when Monday .. Friday => Total_Worked := Total_Worked + Hours_Today;
         when Saturday         => Total_Worked := Total_Worked + Hours_Today * 1.5;
         when Sunday           => Total_Worked := Total_Worked + Hours_Today * 2.0;
      end case;
   end loop Day_Loop;

   Put_Line (Hours_Worked'Image (Total_Worked));
=======
   type Hours is mod 24;
   Start     : Hours;
   Finish    : Hours;
   Big_Block : Boolean;
begin
   Day_Loop :
   for Day in Days_Of_Week_T loop
      case Day is
         when Sunday =>
            Start  := 1;
            Finish := 0;
         when Saturday =>
            Start     := 9;
            Finish    := 13;
            Big_Block := True;
         when Monday .. Friday =>
            Start     := 9;
            Finish    := 17;
            Big_Block := False;
      end case;
      Put_Line (Days_Of_Week_T'Image (Day));
      Put_Line ("======");
      for Hour in Start .. Finish loop
         if Big_Block and then (2 * (Hour / 2) = Hour) then
            New_Line;
         else
            Put_Line ("   " & Hours'Image (Hour) & "00");
         end if;
      end loop;
   end loop Day_Loop;
>>>>>>> origin/master
end Main;
