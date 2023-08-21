with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   --Search
   type List_T is array (Positive range <>) of Integer;

   function Search
     (List : List_T;
      Item : Integer)
      return Positive is
   begin
      if List'Length = 0 then
         return 1;
      elsif Item <= List (List'First) then
         return 1;
      else
         for Idx in (List'First + 1) .. List'Length loop
            if Item <= List (Idx) then
               return Idx;
            end if;
         end loop;
         return List'Last;
      end if;
   end Search;
   --Search

   List   : List_T (1 .. 20);
   Length : Natural := 0;

--Main
   procedure Add (Item : Integer) is
      Place : Natural := Search (List (1..Length), Item);
   begin
      if List (Place) /= Item then
         Length                     := Length + 1;
         List (Place + 1 .. Length) := List (Place .. Length - 1);
         List (Place)               := Item;
      end if;
   end Add;

begin

   Add (100);
   Add (50);
   Add (25);
   Add (50);
   Add (90);
   Add (45);
   Add (22);

   for Idx in 1 .. Length loop
      Put_Line (List (Idx)'Image);
   end loop;

end Main;
--Main
