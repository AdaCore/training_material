package body Generic_List is

   procedure Add
     (This : in out List_T;
      Item : in     Component_T) is
   begin
      null;
   end Add;

   procedure Sort (This : in out List_T) is
      Temp : Component_T;
      --  Set length to number of components in list
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
      null;  --  Print each component in list
   end Print;

end Generic_List;
