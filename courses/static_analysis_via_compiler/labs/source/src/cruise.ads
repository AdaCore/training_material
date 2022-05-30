package Cruise is

   type Mode is (Hard_Braking, Braking, Steady, Speeding, Hard_Speeding);
   subtype Speeding_Mode is Mode range Speeding .. Hard_Speeding;
   subtype Braking_Mode is Mode range Hard_Braking .. Braking;
   subtype Soft_Mode is Mode range Braking .. Speeding;

   type Vehicle is (Self, Front, Rear);
   subtype Other_Vehicle is Vehicle range Front .. Rear;

   type Speed is new Natural range 0 .. 200;    --  Speed in km/h
   type Distance is new Natural range 0 .. 200; --  Distance in m

   type Vehicle_Speeds is array (Vehicle) of Speed;
   type Other_Vehicle_Distances is array (Other_Vehicle) of Distance;

   --  Sensors are polled every 100ms to compute the following information
   --  about the position and speed of the vehicle w.r.t. the preceding (Front)
   --  vehicle and the following (Behind) vehicle
   type State is record
      Speeds        : Vehicle_Speeds;
      Distances     : Other_Vehicle_Distances;
      Front_Braking : Boolean;
   end record;

   --  Depending on the respective positions and speeds of the vehicles (self,
   --  preceding and following ones), decide on proper action to take
   function Control
     (St : State)
      return Mode;

end Cruise;
