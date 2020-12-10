package Radar_Internals is

   -- Types
   type Object_Status_T is (Out_Of_Range, Tracked, Cleared, Selected);
   subtype Object_Distance_Km_T is Float range 0.01 .. 100.0;
   subtype Speed_Kph_T is Float range 0.0 .. 50.0;
   type Speed_Setting_T is (Stopped, Slow, Normal, Fast);

   -- Actions on the Radar
   procedure Rotate_Antenna (Speed : Speed_Setting_T);
   procedure Walk_And_Scan;
   procedure Next_Object;
   procedure Get_Closer (Speed : Speed_Setting_T);

   -- Radar status
   function Get_Active_Object_Status return Object_Status_T;
   function Get_Active_Object_Distance return Object_Distance_Km_T;
   function Get_Running_Speed return Speed_Kph_T;

   -- Radar feedback
   procedure Update_E_T_A (E_T_A : Float);
   procedure Update_No_E_T_A;

   procedure Time_Step;

end Radar_Internals;
