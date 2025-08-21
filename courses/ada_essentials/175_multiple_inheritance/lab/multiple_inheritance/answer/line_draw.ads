--Shapes

--Drawing_Spec
with Base_Types;
package Line_Draw is

   type Object_T is interface;

   -- Create accessor functions for some line attributes
   procedure Set_Color (Object : in out Object_T;
                        Color  : in     Base_Types.Color_T)
         is abstract;
   function Color (Object : Object_T) return Base_Types.Color_T
         is abstract;

   procedure Set_Pen_Width (Object : in out Object_T;
                            Width  : in     Base_Types.Width_T)
         is abstract;
   function Pen_Width (Object : Object_T) return Base_Types.Width_T
         is abstract;

   function Convert (Object : Object_T) return Base_Types.Lines_T
         is abstract;

   procedure Print (Object : Object_T'Class);

end Line_Draw;
