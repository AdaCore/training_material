with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type Days_Of_Week_T is
     (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
   type Hours is mod 24;
   Start     : Hours;
   Finish    : Hours;
   Big_Block : Boolean;
begin
   Day_Loop :
   for Day in Days_Of_Week_T loop
      case Day is
         when Sunday =>
            Start := 1;
            Finish := 0;

         when Saturday =>
            Start := 9;
            Finish := 13;
            Big_Block := True;

         when Monday .. Friday =>
            Start := 9;
            Finish := 17;
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
end Main;
