with Ada.Text_IO; use Ada.Text_IO;

package body Radar_Internals is
   
   T : Natural := 0;

   procedure Time_Step (Radar_Angle        : Float; Time_To_Arrival : Float;
                        John_Connor_Status : String)
   is
      E_T_A_H : Integer
        := Integer (Float'Floor (Time_To_Arrival / 3600.0));
      E_T_A_M : Integer
        := Integer (Float'Floor (Time_To_Arrival / 60.0)) mod 60;
      E_T_A_S : Integer
        := Integer (Float'Rounding (Time_To_Arrival)) mod 60;
   begin
      Put_Line ("T =" & Natural'Image(T)
                & "  "
                & "ETA"
                & Integer'Image (E_T_A_H) & "h"
                & Integer'Image (E_T_A_M) & "m"
                & Integer'Image (E_T_A_S) & "s"
                & "  "
                & "John Connor is " & John_Connor_Status);
      T := T + 1;
   end Time_Step;

end Radar_Internals;

