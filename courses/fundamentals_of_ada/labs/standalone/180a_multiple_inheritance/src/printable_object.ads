
with Geometry;
with Line_Draw;
with Base_Types;
package Printable_Object is

   type Object_T is
     abstract new Geometry.Object_T and Line_Draw.Object_T with private;
   procedure Set_Color
     (Object : in out Object_T;
      Color  :        Base_Types.Color_T);
   function Color
     (Object : Object_T)
      return Base_Types.Color_T;
   procedure Set_Pen
     (Object : in out Object_T;
      Size   :        Positive);
   function Pen
     (Object : Object_T)
      return Positive;

private
   type Object_T is abstract new Geometry.Object_T and Line_Draw.Object_T with
   record
      Color    : Base_Types.Color_T := (0, 0, 0);
      Pen_Size : Positive           := 1;
   end record;

end Printable_Object;
