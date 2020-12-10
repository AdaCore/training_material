with Screen_Interface; use Screen_Interface;
with Trains;

package Tracks_Display is

   type Track_Points is array (Positive range <>) of Point;
   type Track_T;
   type Track_Access is access all Track_T;

   type Switch_States is (S1, S2);
   type Switch is array (Switch_States) of Track_Access;

   type Entry_Sign_Position is (Top, Left, Bottom, Right);
   type Entry_Sign_Color is (Green, Orange, Red);

   type Entry_Sign_T is record
      Coord    : Point := (0, 0);
      Color    : Entry_Sign_Color := Green;
      Disabled : Boolean := false;
   end record;

   type Track_T (Nb_Points : Positive) is record
      Id           : Trains.Track_Opt_Id := Trains.No_Track_Id;
      Entry_Sign   : Entry_Sign_T;
      Points       : Track_Points (1 .. Nb_Points);
      Is_Straight  : Boolean       := False;
      Switchable   : Boolean       := False;
      Switch_State : Switch_States := S1;
      Exits        : Switch;
   end record;

   type Bogie is record
      Track     : Track_Access;
      Track_Pos : Positive;
   end record;

   type Bogie_Array is array (Positive range <>) of Bogie;

   type Train_T (Nb_Bogies : Positive) is record
      Id     : Trains.Train_Id;
      Bogies : Bogie_Array (1 .. Nb_Bogies);
      Speed  : Natural := 0;
   end record;


   procedure Build_Straight_Track (Track : in out Track_T; Start, Stop : Point);
   procedure Build_Curve_Track (Track : in out Track_T; P1, P2, P3, P4 : Point);
   procedure Connect_Track (Track : in out Track_T; E1, E2 : Track_Access);
   function  Start_Coord  (Track : Track_T) return Point;
   function  end_Coord  (Track : Track_T) return Point;
   procedure Set_Sign_Position (Track : in out Track_T;
                                Pos : Entry_Sign_Position);
   procedure Update_Sign (Track : in out Track_T);
   procedure Change_Switch (Track : in out Track_T);

   procedure Draw_Track (Track : Track_T);
   procedure Draw_Switch (Track : Track_T);

   procedure Init_Train (Train : in out Train_T; Track : Track_Access);
   procedure Move_Train (Train : in out Train_T);
   procedure Draw_Train (Train : Train_T);

   function Get_Coords (Bog : Bogie) return Point;
end Tracks_Display;
