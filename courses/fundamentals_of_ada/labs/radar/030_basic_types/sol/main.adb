with Radar_Internals;

procedure Main is
   -- You are in charge of developping a rotating radar for the new T-1000
   -- Some of the radar code is already in place, it is just missing the
   -- high-level interface to handle incoming objects.

   -- QUESTION 1 - Part A
   --
   -- Define a type Object_Status_T that has four possible values
   -- Out_Of_Range, Tracked, Untracked, Selected
   type Object_Status_T is (Out_Of_Range, Tracked, Cleared, Selected);

   -- Define a type Angle_Degrees_T that is modulo 360
   type Angle_Degrees_T is mod 360;

   -- Define a subtype Object_Distance_Km_T as a Float with values
   -- between 10cm and 100km
   subtype Object_Distance_Km_T is Float range 0.01 .. 100.0;

   -- Define a subtype Speed_Kph_T that is a Float between 0 and 50 km/h
   subtype Speed_Kph_T is Float range 0.0 .. 50.0;

   -- QUESTION 1 - Part B
   --
   -- Declare John_Connor, an Object_Status_T with value Out_Of_Range
   John_Connor : Object_Status_T := Out_Of_Range;

   -- Declare Radar_Angle to be an Angle_Degrees_T with a starting value
   Radar_Angle : Angle_Degrees_T := 120;

   -- Declare an Object_Distance_Km_T named Distance_Closest_Object, set to 10km
   Distance_Closest_Object : Object_Distance_Km_T := 10.0;

   -- Declare a Speed_Kph_T named Running_Speed, set to 25km/h
   Running_Speed : Speed_Kph_T := 25.0;

   -- Declare a Float named Time_To_Arrival, calculated as
   -- Distance_Closest_Object / Running_Speed * 3600
   Time_To_Arrival : Float := Distance_Closest_Object / Running_Speed * 3600.0;
begin
   -- This line will compile if the declarations are OK
   Radar_Internals.Time_Step (Float (Radar_Angle), Time_To_Arrival,
                              Object_Status_T'Image (John_Connor));

   -- QUESTION 2 - Part A
   --
   -- Some time has passed since setup, set variables as follow to reflect that.
   --
   -- Rotate the radar 200 degrees by incrementing its value
   Radar_Angle := Radar_Angle + 200;

   -- Set the status of John_Connor to Tracked
   John_Connor := Tracked;

   -- Set distance to closest object to 4km
   Distance_Closest_Object := 4.0;

   -- Update Running_Time accordingly
   Time_To_Arrival := Distance_Closest_Object / Running_Speed * 3600.0;

   -- This line will compile if the declarations are OK
   Radar_Internals.Time_Step (Float (Radar_Angle), Time_To_Arrival,
                              Object_Status_T'Image (John_Connor));

   -- QUESTION 2 - Part B
   --
   -- Some more time has passed since setup.
   --
   -- Rotate the radar 180 degrees
   Radar_Angle := Radar_Angle + 180;

   -- Set the status of John_Connor to Selected
   John_Connor := Selected;

   -- This line will compile if the declarations are OK
   Radar_Internals.Time_Step (Float (Radar_Angle), Time_To_Arrival,
                              Object_Status_T'Image (John_Connor));

   -- QUESTION 3 - Quiz
   --
   -- a. What happens if we want to rotate the radar by 361 degrees?
   -- b. There is a last minute change in the spec: John Connor is know in
   --     a new "Friend" status, make changes to the code to allow for that.
   -- c. What happens to the E.T.A. if Running_Speed is 0? Try it.

   -- QUESTION 4 - Advanced
   --
   -- Redefine Object_Distance_Km_T as a type instead of subtype.
   -- Modify the two division to make it work, using explicit casting.
end Main;
