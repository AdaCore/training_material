with Radar_Internals;

procedure Main is
   -- You are in charge of developping a rotating radar for the new T-1000
   -- Some of the radar code is already in place, it is just missing the
   -- high-level interface to handle incoming objects.

   type Object_Status_T is (Out_Of_Range, Tracked, Cleared, Selected);

   -- QUESTION 1 - Part A
   --
   -- Define a type Angle_Degrees_T that is modulo 360

   -- Define a subtype Object_Distance_Km_T as a Float with values
   -- between 10cm and 100km

   -- Define a subtype Speed_Kph_T that is a Float between 0 and 50 km/h


   John_Connor : Object_Status_T := Out_Of_Range;

   -- QUESTION 1 - Part B
   --
   -- Set Radar_Angle to be an Angle_Degrees_T with a starting value
   Radar_Angle : Integer := 0;

   -- Declare an Object_Distance_Km_T named Distance_Closest_Object, set to 10km

   -- Declare a Speed_Kph_T named Running_Speed, set to 25km/h

   -- Assign Time_To_Arrival to
   -- Distance_Closest_Object divided by Running_Speed * 3600
   Time_To_Arrival : Float := 0.0;

begin
   -- This line will compile if the declarations are OK
   Radar_Internals.Time_Step (Float (Radar_Angle), Time_To_Arrival,
                              Object_Status_T'Image (John_Connor));

   -- QUESTION 2 - Part A
   --
   -- Some time has passed since setup, set variables as follow to reflect that.
   --
   -- Rotate the radar 200 degrees by incrementing its value

   -- Set the status of John_Connor to Tracked

   -- Set distance to closest object to 4km

   -- Update Running_Time accordingly

   -- This line will compile if the declarations are OK
   Radar_Internals.Time_Step (Float (Radar_Angle), Time_To_Arrival,
                              Object_Status_T'Image (John_Connor));

   -- QUESTION 2 - Part B
   --
   -- Some more time has passed since setup.
   --
   -- Rotate the radar 180 degrees

   -- Set the status of John_Connor to Selected

   -- This line will compile if the declarations are OK
   Radar_Internals.Time_Step (Float (Radar_Angle), Time_To_Arrival,
                              Object_Status_T'Image (John_Connor));

   -- QUESTION 3 - Quiz
   --
   -- a. What happens if we want to rotate the radar by 361 degrees?
   -- b. There is a last minute change in the spec: John Connor is now in
   --    the "Friend" status, make changes to the code to allow for that.
   -- c. What happens to the E.T.A. if Running_Speed is 0? Try it.

   -- QUESTION 4 - Advanced
   --
   -- Redefine Object_Distance_Km_T as a type instead of subtype.
   -- Modify the two division to make it work, using explicit casting.
end Main;

-- You can use the 'Scenario' tab on the right to change the Mode from
-- problem to solution, click the checkmark button, and go to the answers
-- directory to compare your solution with the correction.
