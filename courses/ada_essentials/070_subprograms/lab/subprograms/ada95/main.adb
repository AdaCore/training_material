with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   --|definitions_start
   type Coord_T is record
      X_Coord : Integer;
      Y_Coord : Integer;
   end record;

   type Coords_T is array (1 .. 1_000) of Coord_T;
   type List_T is record
      Coords : Coords_T;
      Length : Natural := 0;
   end record;

   procedure Add
     (List  : in out List_T;
      Coord :        Coord_T);
   function Contains
     (List : List_T;
      Item : Coord_T)
      return Boolean;
   procedure Print (List : List_T);
   procedure Print_Element (Coord : Coord_T);

   List : List_T;
   --|definitions_end

   --|implementation_start
   procedure Add
     (List  : in out List_T;
      Coord :        Coord_T) is
   begin
      if not Contains (List, Coord) then
         List.Coords (List.Length + 1) := Coord;
         List.Length                   := List.Length + 1;
      end if;
   end Add;

   function Contains
     (List : List_T;
      Item : Coord_T)
      return Boolean is
   begin
      for Index in 1 .. List.Length loop
         if List.Coords (Index) = Item then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   procedure Print (List : List_T) is
   begin
      New_Line;
      for Index in 1 .. List.Length loop
         Print_Element (List.Coords (Index));
      end loop;
   end Print;

   procedure Print_Element (Coord : Coord_T) is
      function To_String return String is
      begin
         return Integer'Image (Coord.X_Coord) & Integer'Image (Coord.Y_Coord);
      end To_String;
   begin
      Put_Line (To_String);
   end Print_Element;
   --|implementation_end

begin

   Add (List, (1, 2));
   Print (List);
   Add (List, (33, 44));
   Print (List);
   Add (List, (1, 2));
   Print (List);
   Add (List, (555, 666));
   Print (List);

end Main;
--Main
