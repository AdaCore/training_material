generic
   type Component_T is private;
   --  Add other parameters for:
   --    Maximum size of list
   --    Compare to components
   --    Convert component to string
package Generic_List is

   type List_T is private;

   procedure Add
     (This : in out List_T;
      Item : in     Component_T);
   procedure Sort (This : in out List_T);
   procedure Print (List : List_T);

private
   --  Update List_T to be a collection of Component_T
   type List_T is null record;

end Generic_List;
