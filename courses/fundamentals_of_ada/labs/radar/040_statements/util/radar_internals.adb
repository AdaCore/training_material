with Ada.Text_IO; use Ada.Text_IO;

package body Radar_Internals is

   T : Natural := 0;
   
   type Angle_Degrees_T is mod 360;

   Antenna_Angle : Angle_Degrees_T := 120;
   
   type Object_Type_T is (Sarah_Connor, Peon, John_Connor);
   Active_Object_Type : Object_Type_T := Sarah_Connor;
   Active_Object_Status : Object_Status_T := Out_Of_Range; 
   Active_Object_Distance : Object_Distance_Km_T := 9.0;
   
   Current_Walk_Speed : Speed_Setting_T;
   
   Has_E_T_A : Boolean := False;
   Time_To_Arrival : Float := 0.0;
   
   procedure Rotate_Antenna (Speed : Speed_Setting_T) is
   begin
      Antenna_Angle := Antenna_Angle + (case Speed is
                                           when Slow => 7,
                                           when Normal => 17,
                                           when Fast => 59,
                                           when Stopped => 0);
   end Rotate_Antenna;
   
   procedure Walk_And_Scan is
   begin
      Put_Line ("Scanning " & Object_Type_T'Image (Active_Object_Type));
      
      Get_Closer (Slow);
      if Active_Object_Status = Tracked then
         if Active_Object_Type = John_Connor then
            Active_Object_Status := Selected;
         else
            Active_Object_Status := Cleared;
         end if;
      end if; 
   end Walk_And_Scan;
   
   procedure Next_Object is
   begin
      Active_Object_Distance := 9.0;
      Active_Object_Status := Out_Of_Range;
      
      if Active_Object_Type = Object_Type_T'Last then
         Active_Object_Type := Object_Type_T'First;
      else
         Active_Object_Type := Object_Type_T'Val (Object_Type_T'Pos (Active_Object_Type) + 1);
      end if;
   end Next_Object;
   
   procedure Get_Closer (Speed : Speed_Setting_T) is
   begin
      Current_Walk_Speed := Speed;
   end Get_Closer;
   
   function Get_Active_Object_Status return Object_Status_T is
   begin
      return Active_Object_Status;
   end Get_Active_Object_Status;
   
   function Get_Active_Object_Distance return Object_Distance_Km_T is
   begin
      return Active_Object_Distance;
   end Get_Active_Object_Distance;
   
   function Get_Running_Speed return Speed_Kph_T is
   begin
      return (case Current_Walk_Speed is
                 when Stopped => 0.0,
                 when Slow => 4.0,
                 when Normal => 8.0,
                 when Fast => 30.0);
   end Get_Running_Speed;
   
   procedure Update_E_T_A (E_T_A : Float) is
   begin
      Has_E_T_A := True;
      Time_To_Arrival := E_T_A;
   end Update_E_T_A;
   
   procedure Update_No_E_T_A is
   begin
      Has_E_T_A := False;
   end Update_No_E_T_A;
   
   procedure Time_Step is
   begin
      Put ("T =" & Natural'Image(T)
           & "  Antenna" & Angle_Degrees_T'Image (Antenna_Angle)
           & "deg."& ASCII.HT & "ETA");
      if Has_E_T_A then
         declare
            E_T_A_H : Integer
              := Integer (Float'Floor (Time_To_Arrival / 3600.0));
            E_T_A_M : Integer
              := Integer (Float'Floor (Time_To_Arrival / 60.0)) mod 60;
            E_T_A_S : Integer
              := Integer (Float'Rounding (Time_To_Arrival)) mod 60;
         begin
         Put (  Integer'Image (E_T_A_H) & "h"
                     & Integer'Image (E_T_A_M) & "m"
                     & Integer'Image (E_T_A_S) & "s");
         end;
         
         if Time_To_Arrival <= 500.0 then
            Active_Object_Distance := Object_Distance_Km_T'First;
         else
            Active_Object_Distance := Active_Object_Distance
              - Object_Distance_Km_T'Min (Active_Object_Distance - Object_Distance_Km_T'First,
                                          Active_Object_Distance * 500.0 / Time_To_Arrival);
         end if;
         
      else
         Put (" none ");
      end if;
         
      if Active_Object_Distance < 5.0 and Active_Object_Status = Out_Of_Range then
         Active_Object_Status := Tracked;
      end if;
      
      Put_Line (ASCII.HT & Object_Type_T'Image (Active_Object_Type)
                & " is " & Object_Status_T'Image (Active_Object_Status));
      T := T + 1;
   end Time_Step;

end Radar_Internals;
