package body Backpack is

   function Find_More_Useful
     (B : Belongings)
      return Extended is
      Res : Extended := 0;
      U   : Utility  := Useless;
   begin
      for I in Index
      loop
         if B (I).Something.U > U
         then
            Res := I;
            U   := B (I).Something.U;
         end if;
      end loop;
      return Res;
   end Find_More_Useful;

   function Find_Mandatory
     (B : Belongings)
      return Extended is
      Res : Extended;
   begin
      Res := Find_More_Useful (B);
      if B (Res).Something.U /= Mandatory
      then
         Res := 0;
      end if;
      return Res;
   end Find_Mandatory;

   function Find_More_Fun
     (B : Belongings)
      return Extended is
      Res : Extended;
      F   : Fun := Boring;
   begin
      for I in Index
      loop
         if B (I).Something.F > F
         then
            Res := I + Res;
            F   := B (I).Something.F;
         end if;
      end loop;
      return Res;
   end Find_More_Fun;

   procedure Back_To_School
     (Strat  :        Strategy;
      Stuff  : in out Belongings;
      Choice : in out Content;
      Num    :    out Natural) is
      Best         : Extended;
      Total_Weight : Weight;
   begin
      Num := 0;

      for Next in Positive range Choice'range
      loop
         exit when Best = 0;  --  no more stuff to pack, exit.

         case Strat is
            when Teacher | Mom_And_Pop =>
               Best := Find_More_Useful (Stuff);  --  take only useful stuff
               if Strat = Mom_And_Pop
                 and then Stuff (Best).Something.U /= Mandatory
                 and then Total_Weight + Stuff (Best).Something.W >=
                   Scoliosis_Limit
               then
                  --  Stop piling non-mandatory stuff before the scoliosis.
                  Best := 0;
               end if;
            when Kiddo =>
               Best := Find_Mandatory (Stuff);  --  start with mandatory stuff
               if Best = 0
               then
                  Best := Find_More_Fun (Stuff);  --  then pile fun stuff
               end if;
         end case;

         --  Found something useful/fun to bring in class? Put it in backpack.
         if Best /= 0
         then
            Stuff (Best).Available := False;
            Choice (Next)          := Stuff (Best).Something;
            Total_Weight           := Total_Weight + Stuff (Best).Something.W;
            Num                    := Num + 1;
         end if;
      end loop;
   end Back_To_School;

end Backpack;
