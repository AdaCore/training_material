package Cruise.Legacy is

   type Mode is (Hard_Braking, Braking, Steady, Speeding, Hard_Speeding);
   subtype Soft_Mode is Mode range Braking .. Speeding;

   type Vehicle is (Self, Front, Rear);
   type Vehicle_Speeds is array (Vehicle) of Integer;
   type Other_Vehicle_Distances is array (-1 .. 1) of Integer;

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
   function Control (ST : State) return Mode;

end Cruise.Legacy;
