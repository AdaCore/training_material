with Base_Types;
package Line_Draw is
   type Object_T is interface;
   -- Create accessor functions for some line attributes
   -- (e.g. color, pen width)
   function Convert
     (Object : Object_T)
      return Base_Types.Lines_T is abstract;
   procedure Print (Object : Object_T'Class);
end Line_Draw;
