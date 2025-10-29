with Types; use Types;
package Color_Set is

   type Color_List_T is array (Positive range <>) of Color_T;
   type Color_Set_T is private;

   Empty_Set : constant Color_Set_T;

   function Create (Colors : Color_List_T) return Color_Set_T;
   function Contains (Colors : Color_Set_T;
                      Color  : Color_T)
                      return Boolean;
   function Image (Set : Color_Set_T) return String;

private
   type Color_Set_Array_T is array (Color_T) of Boolean;
   type Color_Set_T is record
      Values : Color_Set_Array_T := (others => False);
   end record;
   Empty_Set : constant Color_Set_T :=
     (Values => (others => False));
end Color_Set;
