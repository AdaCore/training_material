package body Cruise is

   --  Return the distance (in m) covered in 2s at speed S (in km/h)
   function Security_Distance (S : Speed) return Distance is
   begin
      return Distance (S * 5 / 9);
   end Security_Distance;

   --  Prevent collision with preceding vehicle by braking appropriately
   function Control_Braking (Front_Braking : Boolean) return Braking_Mode is
   begin
      if Front_Braking then
	 return Hard_Braking;
      else
	 return Braking;
      end if;
   end Control_Braking;

   --  Prevent collision with following vehicle by speeding appropriately
   function Control_Speeding
      (Self_Speed     : Speed;
       Front_Distance : Distance) return Speeding_Mode is
   begin
      if Front_Distance < Security_Distance (Self_Speed) then
	 return Speeding;
      else
	 return Hard_Speeding;
      end if;
   end Control_Speeding;

   --  Optimize position of vehicle w.r.t. preceding and following vehicles
   --  to balance the risks of collision with both vehicles
   function Control_Position (ST : State) return Soft_Mode is
      Front_Speed           : Speed    := ST.Speeds (Front);
      Front_Distance        : Distance := ST.Distances (Front);
      Self_Speed            : Speed    := ST.Speeds (Self);
      Rear_Speed            : Speed    := ST.Speeds (Front);
      Rear_Distance         : Distance := ST.Distances (Rear);
      Time_Collision_Front  : Float    := -1.0;
      Time_Collision_Rear   : Float    := -1.0;
   begin
      if Front_Speed < Self_Speed then
	 Time_Collision_Front :=
	   Float (Front_Distance) / Float (Self_Speed - Front_Speed);
      end if;
      if Self_Speed < Rear_Speed then
	 Time_Collision_Rear :=
	   Float (Rear_Distance) / Float (Rear_Speed - Self_Speed);
      end if;
      if Time_Collision_Front < 0.0 and then Time_Collision_Rear < 0.0 then
         --  No risk of collision
	 return Steady;
      elsif Time_Collision_Front >= 0.0 and then Time_Collision_Rear >= 0.0
      then
         --  Risk of collision with both preceding and following vehicles
	 if Time_Collision_Front < Time_Collision_Rear then
	    return Braking;
	 else
	    return Speeding;
	 end if;
      elsif Time_Collision_Front < 0.0 then
         --  Risk of collision with following vehicle only
	 return Speeding;
      else
         --  Risk of collision with preceding vehicle only
	 return Braking;
      end if;
   end Control_Position;

   function Control (ST : State) return Mode is
      Front_Speed     : Speed    := ST.Speeds (Front);
      Front_Distance  : Distance := ST.Distances (Front);
      Front_Braking   : Boolean  := ST.Front_Braking;
      Self_Speed      : Speed    := ST.Speeds (Self);
      Rear_Speed      : Speed    := ST.Speeds (Rear);
      Rear_Distance   : Distance := ST.Distances (Rear);
      Sec_Dist_Front  : Distance := Security_Distance (Self_Speed);
      Sec_Dist_Rear   : Distance := Security_Distance (Rear_Speed);
   begin
      if Rear_Speed < Self_Speed and then
        Front_Distance < Sec_Dist_Front then
         --  Brake if preceding vehicle is close and we are approaching it
         return Control_Braking (Front_Braking => Front_Braking);
      elsif Self_Speed < Rear_Speed or else
        Rear_Distance < Sec_Dist_Rear then
         --  Speed-up if following vehicle is close and approaching us
         return Control_Speeding (Self_Speed     => Self_Speed,
                                  Front_Distance => Front_Distance);
      elsif Front_Distance < Sec_Dist_Front or else
        Rear_Distance < Sec_Dist_Rear then
         --  Maintain equivalent time-to-impact with preceding and following
         --  vehicles if both are close
	 return Control_Position (ST);
      else
	 return Steady;
      end if;
   end Control;

end Cruise;
