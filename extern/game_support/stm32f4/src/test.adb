with Screen_Interface; use Screen_Interface;
with Drawing; use Drawing;
with Ada.Real_Time; use Ada.Real_Time;

procedure Test is
   Period : constant Time_Span := Milliseconds (500);
   Next_Start : Time := Clock + Seconds (1);
begin
   Screen_Interface.Initialize;
   Fill_Screen (Black);
   Circle (Center => (100, 100),
           Radius => 50,
           Col    => Green,
           Fill   => True);
   loop
      Next_Start := Next_Start + Period;
      delay until Next_Start;
   end loop;
end Test;
