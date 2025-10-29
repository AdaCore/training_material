with Types; use Types;
package Color_Set is

   type Color_List_T is array (Positive range <>) of Color_T;
   type Color_Set_T is new Integer;  -- implement this!

   function Create
     (Colors : Color_List_T)
      return Color_Set_T;
   function Contains
     (Colors : Color_Set_T;
      Color  : Color_T)
      return Boolean;
   function Image
     (Set : Color_Set_T)
      return String;

end Color_Set;
