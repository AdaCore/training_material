with Geometry;
with Line_Draw;
with Base_Types;

package Printable_Object is
   type Object_T is abstract
     new Geometry.Object_T
     and Line_Draw.Object_T with private;
   procedure Set_Color (Object : in out Object_T; Color : Base_Types.Color_T);
   function Color (Object : Object_T) return Base_Types.Color_T;

   procedure Set_Pen_Width
     (Object : in out Object_T; Width : Base_Types.Width_T);
   function Pen_Width (Object : Object_T) return Base_Types.Width_T;
private
   type Object_T is abstract new Geometry.Object_T and Line_Draw.Object_T
   with record
      The_Color     : Base_Types.Color_T := 0;
      The_Pen_Width : Base_Types.Width_T := 1;
   end record;
end Printable_Object;
