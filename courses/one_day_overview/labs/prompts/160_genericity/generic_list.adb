package body Generic_List is

   procedure Add
     (This : in out List_T;
      Item : in     Element_T) is
   begin
      null;
   end Add;

   procedure Sort (This : in out List_T) is
      Temp : Element_T;
      --  Set length to number of elements in list
      Length : constant Integer := 0;
   begin
      for I in 1 .. Length
      loop
         for J in 1 .. Length - I
         loop
            null;
            --  If current item is greater than next item
            --     Swap current item and next item
         end loop;
      end loop;
   end Sort;

   procedure Print (List : List_T) is
   begin
      null;  --  Print each element in list
   end Print;

end Generic_List;
