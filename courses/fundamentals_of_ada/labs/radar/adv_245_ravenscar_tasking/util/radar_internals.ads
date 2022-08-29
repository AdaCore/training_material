package Radar_Internals is

   type Angle_Degrees_T is mod 360;

   type Object_Type_T is (Sarah_Connor, Peon, John_Connor);
   type Object_Status_T is (Out_Of_Range, Tracked, Cleared, Selected);

end Radar_Internals;
