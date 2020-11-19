-----------------
Basic Types Lab
-----------------

* You are in charge of developping a new radar for the T-1000

    - The rotating radar must sort friends from foes
    - The T-1000 will get close to objects to identify them
    - Once an object is targeted, the fire-safety goes off
    - We give each object around the radar a status:

    .. image:: images/030_basic_types_lab_radar.png

* Create types to handle the following concepts

    - Status of an object
    - Radar antenna angle in degrees
    - Distance to an object in kilometers
    - Time for the T-1000 to get to cover this distance (ETA)

* Declare objects, print their values, modify them

    - Keep up to date the distance (and ETA) to the closest tracked object
    - Rotate the radar antenna by a full circle
    - Change the status of some objects (update tracking info accordingly)

-----------------------
Basic Types Lab Hints
-----------------------

* This is (still) an exercice in declaration

    - A little imagination needed
    - Flow-control lessons are coming soon...
    - For the moment don't be afraid of copy-pasting-adapting code

* It is a good practice to suffix type names (eg with `_T`)
* Make full use of the Ada typing system

    - Do you want to define a set of choices, or a cardinality?
    - Which precision do you expect?
    - What about zero and negative values?
    - Do you want the number to wrap-around?

* Print values with standard package `Ada.Text_IO`

    - Procedure `Put_Line (S : String)`
    - Attribute `'Image` returns a `String`

    .. code:: Ada

      <typemark>'Image ( object )
      Object'Image

--------------------------
Basic Types Extra Credits
--------------------------

* Handle invalid / illegal data and states

    - If all objects are out-of-range, what about tracking info?
    - Distance = 0
    - Radar angle < 0
    - Rotate the antenna by more than 360 degrees
    - If the T-1000 gets immobilized, what about ETA ?

----------------------------------------
Basic Types Lab Solution (Definitions)
----------------------------------------

.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Main is
      type Object_Status_T is (Out_Of_Range, Tracked, Untracked, Targeted);
      type Angle_Degrees_T is mod 360;
      subtype Object_Distance_Km_T is Float range 0.01 .. Float'Last;
      subtype Seconds_T is Float range 0.0 .. Float'Last;

      John_Connor : Object_Status_T := Out_Of_Range;
      Sarah_Connor : Object_Status_T := Tracked;
      Peon : Object_Status_T := Untracked;

      Radar_Angle : Angle_Degrees_T := 120;
      Fire_Safety : Boolean := True;
      Distance_Closest_Object : Object_Distance_Km_T := 10.0;
      T1000_Running_Speed_m_P_S : Float := 30.0 / 3600.0;
      Running_Time : Seconds_T
        := Distance_Closest_Object / T1000_Running_Speed_m_P_S;

------------------------------------------
Basic Types Lab Solution (Implementation)
------------------------------------------

.. code:: Ada

   begin
      Ada.Text_IO.Put_Line ("T=0");
      Ada.Text_IO.Put_Line (Object_Status_T'Image (John_Connor));
      Ada.Text_IO.Put_Line (Angle_Degrees_T'Image (Radar_Angle));
      Ada.Text_IO.Put_Line (Float'Image (Running_Time));

      Ada.Text_IO.Put_Line ("T=1300");
      Radar_Angle := Radar_Angle + 200;
      Sarah_Connor := Untracked;
      John_Connor := Tracked;
      Distance_Closest_Object := 4.0;
      Running_Time
        := Distance_Closest_Object / T1000_Running_Speed_m_P_S;
      Ada.Text_IO.Put_Line (Object_Status_T'Image (John_Connor));
      Ada.Text_IO.Put_Line (Angle_Degrees_T'Image (Radar_Angle));
      Ada.Text_IO.Put_Line (Float'Image (Running_Time));

      Ada.Text_IO.Put_Line ("T=2300");
      Radar_Angle := Radar_Angle + 180;
      John_Connor := Targeted;
      Distance_Closest_Object := 0.5;
      Running_Time
        := Distance_Closest_Object / T1000_Running_Speed_m_P_S;
      Ada.Text_IO.Put_Line (Object_Status_T'Image (John_Connor));
      Ada.Text_IO.Put_Line (Angle_Degrees_T'Image (Radar_Angle));
      Ada.Text_IO.Put_Line (Float'Image (Running_Time));
   end Main;

