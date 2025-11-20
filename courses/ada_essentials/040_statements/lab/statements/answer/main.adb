with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   type Days_Of_Week_T is
     (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
   type Hours is mod 24;
   Start     : Hours;
   Finish    : Hours;
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
         when Monday .. Friday =>
            Start     := 9;
            Finish    := 17;
      end case;
      Put_Line (Day'Image);
      Put_Line ("======");
      if Finish < Start then
         Put_Line ("   No work");
      else
         for Hour in Start .. Finish loop
            Put_Line ("   " & Hour'Image & "00");
         end loop;
      end if;
   end loop Day_Loop;
end Main;
