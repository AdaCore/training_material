with Base_Types;
package Geometry is

   -- Create a tagged type to define shapes
   type Object_T is abstract tagged private;

   -- Create accessor functions for some common component
   function Origin (Object : Object_T'Class) return Base_Types.Coordinate_T;

private

   type Object_T is abstract tagged record
      The_Origin : Base_Types.Coordinate_T;
   end record;

   function Origin (Object : Object_T'Class) return Base_Types.Coordinate_T is
      (Object.The_Origin);

end Geometry;
