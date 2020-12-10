with Drawing; use Drawing;

package body Tracks_Display is

   Entry_Sign_Size : constant := 6;
   Entry_Sign_Pixel : constant array (Entry_Sign_Color) of Color
     := (Green => Screen_Interface.Green,
         Orange => Screen_Interface.Orange,
         Red => Screen_Interface.Red);

   Track_Color : constant Color := Screen_Interface.Light_Gray;
   Track_Thickness : constant := 4;
   Train_Thickness : constant := 2;
   Switch_Color : constant Color := Screen_Interface.Violet;
   Switch_Thickness : constant := 2;

   use type Trains.Train_Id;

   function First_Bogie_Track (Train : Train_T) return Trains.Track_Id;
   function Last_Bogie_Track (Train : Train_T) return Trains.Track_Id;

   procedure Build_Straight_Track (Track : in out Track_T;
                                   Start, Stop : Point)
   is
      Diff_X : constant Float := (Float(Stop.X) - Float(Start.X))
        / (Float (Track.Points'Length) - 1.0);

      Diff_Y : constant Float := (Float(Stop.Y) - Float(Start.Y))
        / (Float (Track.Points'Length) - 1.0);

   begin
      for I in Track.Points'Range loop
         declare
            T : constant Float := (Float (I) - 1.0);
         begin
            Track.Points (I).X := Width (Float (Start.X) + T * Diff_X);
            Track.Points (I).Y := Height (Float (Start.Y) + T * Diff_Y);
         end;
      end loop;
      Track.Is_Straight := True;

      --  Default Sign Position
      Set_Sign_Position (Track, Top);
   end Build_Straight_Track;

   procedure Build_Curve_Track (Track : in out Track_T;
                                P1, P2, P3, P4 : Point) is
   begin
      for I in Track.Points'Range loop
         declare
            T : constant Float := Float (I) / Float (Track.Points'Length);
            A : constant Float := (1.0 - T)**3;
            B : constant Float := 3.0 * T * (1.0 - T)**2;
            C : constant Float := 3.0 * T**2 * (1.0 - T);
            D : constant Float := T**3;
         begin
            Track.Points (I).X := Width (A * Float (P1.X) +
                                         B * Float (P2.X) +
                                         C * Float (P3.X) +
                                         D * Float (P4.X));
            Track.Points (I).Y := Height (A * Float (P1.Y) +
                                          B * Float (P2.Y) +
                                          C * Float (P3.Y) +
                                          D * Float (P4.Y));
         end;
      end loop;
      Track.Is_Straight := False;

      --  Fix first and last coordinate
      Track.Points (Track.Points'First) := P1;
      Track.Points (Track.Points'Last) := P4;

      --  Default Sign Position
      Set_Sign_Position (Track, Top);
   end Build_Curve_Track;

   procedure Set_Sign_Position (Track : in out Track_T;
                                Pos : Entry_Sign_Position)
   is
      First : constant Point := Track.Points (Track.Points'First);
      Coord  : Point;
   begin
      case Pos is
         when Top =>
            Coord := (First.X - Width (Entry_Sign_Size * 1.5), First.Y);
         when Left =>
            Coord := (First.X, First.Y + Height (Entry_Sign_Size * 1.5));
         when Bottom =>
            Coord := (First.X + Width (Entry_Sign_Size * 1.5), First.Y);
         when Right =>
            Coord := (First.X, First.Y - Height (Entry_Sign_Size * 1.5));
      end case;

      Track.Entry_Sign.Coord := Coord;
   end Set_Sign_Position;

   procedure Connect_Track (Track : in out Track_T; E1, E2 : Track_Access) is
   begin
      if E1 = null then
         raise Program_Error;
      else
         Track.Exits (S1) := E1;
         Track.Switch_State := S1;
         Track.Switchable := False;

         --  Connected track should share point coordinate
         if Track.Points (Track.Points'Last) /=
           E1.all.Points (E1.all.Points'First) then
            raise Program_Error;
         end if;

      end if;

      if E2 /= null then
         Track.Exits (S2) := E2;
         Track.Switchable := True;
         E2.Entry_Sign.Disabled := True;

         --  Connected track should share point coordinate
         if Track.Points (Track.Points'Last) /=
           E2.all.Points (E2.all.Points'First) then
            raise Program_Error;
         end if;

      end if;
   end Connect_Track;

   procedure Init_Train (Train : in out Train_T; Track : Track_Access) is
      Cnt : Natural := 1;
   begin
      if Train.Nb_Bogies > Track.all.Points'Length then
         --  Not enough space to put the train.
         raise Program_Error;
      end if;

      Train.Id := Trains.Cur_Num_Trains;

      for Bog of Train.Bogies loop
         Bog.Track := Track;
         Bog.Track_Pos := Track.all.Points'First + Train.Nb_Bogies - Cnt;

         if Bog.Track_Pos not in Track.Points'Range then
            raise Program_Error;
         end if;
         Cnt := Cnt + 1;
      end loop;
   end Init_Train;

   function Next_Track (Track : Track_Access) return Track_Access is
   begin
      if Track.all.Switchable
        and then Track.all.Switch_State = S2
      then
         return Track.all.Exits (S2);
      else
         return Track.all.Exits (S1);
      end if;
   end Next_Track;

   --  Return True when the bogie moves to the entry of a new track
   procedure Move_Bogie (Bog : in out Bogie) is
   begin
      if Bog.Track_Pos = Bog.Track.all.Points'Last then
         Bog.Track := Next_Track (Bog.Track);

         --  The first point of track has the same position has the last of
         --  previous tack, so we skip it.
         Bog.Track_Pos := Bog.Track.all.Points'First + 1;
      else
         Bog.Track_Pos := Bog.Track_Pos + 1;
      end if;
   end Move_Bogie;

   procedure Move_Train (Train : in out Train_T) is
      use type Trains.Move_Result;
      Cnt : Integer := 0;
      Train_Copy : Train_T (Train.Nb_Bogies);
      Sign_Command : Trains.Move_Result;
   begin
      loop
         --  Make a copy of the train an move it
         Train_Copy := Train;

         --  Each bogie takes the previous position of the bogie in front him
         for Index in reverse Train_Copy.Bogies'First + 1 .. Train_Copy.Bogies'Last loop
            Train_Copy.Bogies (Index) := Train_Copy.Bogies (Index - 1);
         end loop;

         --  To move the first bogie we need to see if we are at the end of a
         --  track and maybe a switch.
         Move_Bogie (Train_Copy.Bogies (Train_Copy.Bogies'First));

         --  Check if that move was legal
         Trains.Move (Train_Copy.Id,
                      (First_Bogie_Track (Train_Copy),
                       0,
                       Last_Bogie_Track (Train_Copy)),
                      Sign_Command);

         if Sign_Command /= Trains.Stop then
            --  Redraw the track under the last bogie this is an optimisation to
            --  avoid redrawing all tracks at each loop.
            Line (Get_Coords (Train.Bogies (Train.Bogies'Last)),
                  Get_Coords (Train.Bogies (Train.Bogies'Last - 1)),
                  Track_Color, Track_Thickness);

            --  This move is ilegal, set train to the new position
            Train := Train_Copy;
         end if;

         case Sign_Command is
            when Trains.Full_Speed =>
               Train.Speed := 3;
            when Trains.Slow_Down =>
               Train.Speed := 1;
            when Trains.Stop =>
               Train.Speed := 0;
            when Trains.Keep_Going =>
               --  Keep the same speed
               null;
         end case;

         exit when Train.Speed <= Cnt;

         Cnt := Cnt + 1;
      end loop;
   end Move_Train;

   procedure Draw_Sign (Track : Track_T) is
   begin
      if (Track.Entry_Sign.Coord /= (0, 0)) then
         if not Track.Entry_Sign.Disabled then
            Circle (Track.Entry_Sign.Coord, Entry_Sign_Size / 2,
                    Entry_Sign_Pixel (Track.Entry_Sign.Color), True);
         else
            --  Draw a black circle to "erase" the previous drawing
            Circle (Track.Entry_Sign.Coord, Entry_Sign_Size / 2, Black, True);
         end if;
      end if;
   end Draw_Sign;

   procedure Draw_Track (Track : Track_T) is
   begin

      if Track.Is_Straight then
         Line (Track.Points (Track.Points'First),
               Track.Points (Track.Points'Last),
               Track_Color, Track_Thickness);
      else
         for Index in Track.Points'First .. Track.Points'Last - 1 loop
            Line (Track.Points (Index),
                  Track.Points (Index + 1),
                  Track_Color, Track_Thickness);
         end loop;
      end if;

      Draw_Sign (Track);
   end Draw_Track;

   procedure Draw_Switch (Track : Track_T) is
      Target : constant Track_Access := Track.Exits (Track.Switch_State);
   begin
      if Track.Switchable then
         For Cnt in Target.Points'First .. Target.Points'First + 10 loop
            declare
               P1 : constant Point := Target.all.Points (Cnt);
               P2 : constant Point := Target.all.Points (Cnt + 1);
            begin
               Line (P1, P2, Switch_Color, Switch_Thickness);
            end;
         end loop;
      end if;
   end Draw_Switch;

   procedure Draw_Train (Train : Train_T) is
      Train_Color : Color;
   begin
      for Index in Train.Bogies'First .. Train.Bogies'Last - 1 loop
         declare
            B1     : constant Bogie := Train.Bogies (Index);
            Track1 : constant Track_Access := B1.Track;
            P1     : constant Point := Track1.Points (B1.Track_Pos);
            B2     : constant Bogie := Train.Bogies (Index + 1);
            Track2 : constant Track_Access := B2.Track;
            P2     : constant Point := Track2.Points (B2.Track_Pos);
         begin
            case Train.Speed is
               when 0 =>
                  Train_Color := Screen_Interface.Red;
               when 1 =>
                  Train_Color := Screen_Interface.Orange;
               when others =>
                  Train_Color := Screen_Interface.Black;
            end case;
            Line (P1, P2, Train_Color, Train_Thickness);
         end;
      end loop;
   end Draw_Train;

   procedure Update_Sign (Track : in out Track_T) is
      Prev_Color : constant Entry_Sign_Color := Track.Entry_Sign.Color;
   begin
      case Trains.Track_Signals (Track.Id) is
         when Trains.Green =>
            Track.Entry_Sign.Color := Green;
         when Trains.Orange =>
            Track.Entry_Sign.Color := Orange;
            Draw_Track (Track);
         when Trains.Red =>
            Track.Entry_Sign.Color := Red;
      end case;
      if Track.Entry_Sign.Color /= Prev_Color then
         Draw_Sign (Track);
      end if;
   end Update_Sign;

   procedure Change_Switch (Track : in out Track_T) is
   begin
      if Track.Switch_State = S1 then
         Track.Switch_State := S2;
         Track.Exits (S1).Entry_Sign.Disabled := True;
         Track.Exits (S2).Entry_Sign.Disabled := False;
      else
         Track.Switch_State := S1;
         Track.Exits (S2).Entry_Sign.Disabled := True;
         Track.Exits (S1).Entry_Sign.Disabled := False;
      end if;

      Draw_Track (Track.Exits (S1).all);
      Draw_Track (Track.Exits (S2).all);
      Draw_Track (Track);
   end Change_Switch;

   function Get_Coords (Bog : Bogie) return Point is
   begin
      return Bog.Track.Points (Bog.Track_Pos);
   end Get_Coords;

   function First_Bogie_Track (Train : Train_T) return Trains.Track_Id is
      Bog : constant Bogie := Train.Bogies(Train.Bogies'First);
   begin
      return Bog.Track.Id;
   end First_Bogie_Track;

   function Last_Bogie_Track (Train : Train_T) return Trains.Track_Id is
      Bog : constant Bogie := Train.Bogies(Train.Bogies'Last);
   begin
      return Bog.Track.Id;
   end Last_Bogie_Track;

   function  Start_Coord  (Track : Track_T) return Point is
   begin
      return Track.Points (1);
   end Start_Coord;

   function  end_Coord  (Track : Track_T) return Point is
   begin
      return Track.Points (Track.Points'Last);
   end end_Coord;

end Tracks_Display;
