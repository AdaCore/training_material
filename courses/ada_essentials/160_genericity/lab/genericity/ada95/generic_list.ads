generic
   type Component_T is private;
   Max_Size : Natural;
   with function ">" (L, R : Component_T) return Boolean;
   with function Image (Component : Component_T) return String;
package Generic_List is

   type List_T is private;

   procedure Add (This : in out List_T; Item : in Component_T);
   procedure Sort (This : in out List_T);
   procedure Print (List : List_T);

private
   subtype Index_T is Natural range 0 .. Max_Size;
   type List_Array_T is array (1 .. Index_T'Last) of Component_T;

   type List_T is record
      Values : List_Array_T;
      Length : Index_T := 0;
   end record;
end Generic_List;
