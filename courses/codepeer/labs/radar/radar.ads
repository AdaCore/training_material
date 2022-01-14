package Radar is

   type Friend_Or_Foe is (Friend, Foe, Unknown);
   type Class_Of_Ship is
     (Not_A_Ship, Coastal_Patrol, Fast_Attack_Craft, Corvette, Frigate,
      Destroyer, Cruiser, Pocket_Battleship, Battlecruiser, Helicopter_Carrier,
      Battleship, Dreadnought, Aircraft_Carrier, Supercarrier);
   type Kilometer is new Natural;

   --  Every ship detected by the radar is assigned a unique identifier that
   --  allows tracking it until it espaces the radar's scope.
   type Ship is record
      Quality   : Friend_Or_Foe := Unknown;
      Category  : Class_Of_Ship := Not_A_Ship;
      Distance  : Kilometer     := 0;
      Unique_ID : Natural       := 0;
   end record;

   function "=" (X, Y : Ship) return Boolean;

   type Ship_Array is array (Positive range <>) of Ship;

   --  Detect when too many ships are sailing in the combat zone, which makes
   --  it very likely to be hit by enemy fire or friendly fire. This triggers
   --  a different combat mode where the ship principally attempts to escape
   --  the crowded zone.
   Saturated_Combat_Zone : Boolean := False;

   --  Track ships closer than this distance
   Short_Distance : constant Kilometer := 20;

   --  Short list of nearest ships that should be dealt with higher priority,
   --  whether it is to engage combat for enemies or to agree on maneuvers
   --  for friends.
   protected type Short_List (Max_Tracked_Ships : Positive) is
      --  Add a ship when entering the short range
      procedure Add_Ship (E : Ship);
      --  Remove a ship when leaving the short range
      procedure Remove_Ship (E : Ship);
      --  Return the biggest ship in the short range
      function Biggest_Ship return Ship;
   private
      Num_Tracked_Ships   : Natural := 0;
      Num_Untracked_Ships : Natural := 0;
      --  Only tracked ships are included in Ships
      Ships               : Ship_Array (1 .. Max_Tracked_Ships);
   end Short_List;

   Friends : Short_List (50);  --  50 friendly ships tracked for maneuvers
   Foes    : Short_List (18);  --  18 enemy ships tracked (# of weapon systems)

   --  Track a ship entering and leaving the short range
   task type Track_Ship is
      entry New_Ship (E : Ship);
      entry Update_Position (Distance : Kilometer);
   end Track_Ship;

end Radar;

