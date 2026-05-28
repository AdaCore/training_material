with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   type Coord_T is record
      X_Coord : Integer;
      Y_Coord : Integer;
   end record;

   type Coords_T is array (1 .. 1_000) of Coord_T;
   type List_T is record
      Coords : Coords_T;
      Length : Natural := 0;
   end record;

   -- Add specifications for your subprograms here

   List : List_T;

   -- Add bodies for your subprograms here

begin

   null;
   --  Your subprograms should be able to handle the following
   --  procedure calls (after you uncomment them!)

   --  Add (List, (1, 2));
   --  Print (List);
   --  Add (List, (33, 44));
   --  Print (List);
   --  Add (List, (1, 2));
   --  Print (List);
   --  Add (List, (555, 666));
   --  Print (List);

end Main;
