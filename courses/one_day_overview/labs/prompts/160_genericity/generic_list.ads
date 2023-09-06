generic
   type Element_T is private;
   --  Add other parameters for:
   --    Maximum size of list
   --    Compare to elements
   --    Convert element to string
package Generic_List is

   type List_T is private;

   procedure Add
     (This : in out List_T;
      Item : in     Element_T);
   procedure Sort (This : in out List_T);
   procedure Print (List : List_T);

private
   --  Update List_T to be a collection of Element_T
   type List_T is null record;

end Generic_List;
