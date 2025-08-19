--Graphics_Body

--Printable_Object
with Geometry;
with Line_Draw;
with Base_Types;
package Printable_Object is
   type Object_T is abstract tagged private;
   -- Create accessor functions for some attributes
   procedure Set_Attribute
     (Object    : in out Object_T;
      Attribute :        Integer);
   function Get_Attribute
     (Object : Object_T)
      return Integer;
private
   -- Implement Object_T
   type Object_T is abstract tagged null record;
end Printable_Object;
