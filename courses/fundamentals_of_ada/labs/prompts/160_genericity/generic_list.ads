generic
   -- generic parameters for:
   --    List element
   --    Maximum size of list
   --    Subprogram to compare list elements
package Generic_List is

   type List_T is tagged private;

   -- Need a subprogram to add items to the list
   --  procedure Add (This : in out List_T;
   --                 Item : in     ?

   procedure Sort (This : in out List_T);

private
   -- Finish implementation of List_T
   type List_T is tagged null record;
end Generic_List;
