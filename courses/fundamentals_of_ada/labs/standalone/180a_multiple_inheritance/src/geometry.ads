
with Base_Types;
package Geometry is

   type Object_T is abstract tagged private;
   function Origin
     (Object : Object_T'Class)
      return Base_Types.Coordinate_T;

private
   type Object_T is abstract tagged record
      Origin : Base_Types.Coordinate_T;
   end record;

   function Origin
     (Object : Object_T'Class)
      return Base_Types.Coordinate_T is (Object.Origin);

end Geometry;
