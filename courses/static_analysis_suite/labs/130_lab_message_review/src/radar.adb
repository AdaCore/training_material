package body Radar is

   function "=" (X, Y : Ship) return Boolean is
   begin
      return X.Unique_ID = Y.Unique_ID;
   end "=";

   protected body Short_List is

      procedure Add_Ship (E : Ship) is
      begin
         if Num_Tracked_Ships < Max_Tracked_Ships then
            Ships (Num_Tracked_Ships) := E;
            Num_Tracked_Ships         := Num_Tracked_Ships + 1;
         else
            Num_Untracked_Ships       := Num_Untracked_Ships + 1;
            Saturated_Combat_Zone     := True;
         end if;
      end Add_Ship;

      procedure Remove_Ship (E : Ship) is
      begin
         for J in Ships'First .. Num_Tracked_Ships loop
            if Ships (J) = E then
               --  Ship E was found. First remove it.
               for K in J + 1 .. Num_Tracked_Ships loop
                  Ships (K - 1) := Ships (K);
               end loop;
               --  Then update the number of tracked ships and return.
               Num_Tracked_Ships := Num_Tracked_Ships - 1;
               return;
            end if;
         end loop;

         --  Ship E was not found, so it is an untracked ship.
         --  Update the number of untracked ships and possibly leave the mode
         --  where combat zone is saturated.
         if Num_Untracked_Ships = 0 then
            Saturated_Combat_Zone := False;
         end if;
         Num_Untracked_Ships := Num_Untracked_Ships - 1;
      end Remove_Ship;

      function Biggest_Ship return Ship is
         Current : Class_Of_Ship := Not_A_Ship;
         Index   : Integer;
      begin
         for J in Ships'First .. Num_Tracked_Ships loop
            if Current < Ships (J).Category then
               --  Found a bigger ship. Update Current and Index.
               Current := Ships (J).Category;
               Index   := J;
            end if;
         end loop;

         return Ships (Index);
      end Biggest_Ship;

   end Short_List;

   task body Track_Ship is
      Current : Ship;
   begin
      --  Define the ship being tracked.
      accept New_Ship (E : Ship)
      do
         Current := E;
      end New_Ship;

      loop
         --  Following a movement of the ship entering or leaving the short
         --  range, add or remove it from the corresponding short list.
         accept Update_Position (Distance : Kilometer)
         do
            Current.Distance := Distance;
            if Distance < Short_Distance then
               case Current.Quality is
                  when Friend =>
                     Friends.Add_Ship (Current);
                  when Foe =>
                     Foes.Add_Ship (Current);
                  when Unknown =>
                     null;
               end case;
            else
               case Current.Quality is
                  when Friend =>
                     Friends.Remove_Ship (Current);
                  when Foe =>
                     Foes.Remove_Ship (Current);
                  when Unknown =>
                     null;
               end case;
            end if;
         end;
      end loop;
   end Track_Ship;

end Radar;
