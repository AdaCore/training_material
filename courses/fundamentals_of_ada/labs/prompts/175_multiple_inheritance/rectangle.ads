--Printable_Object

--Rectangle
with Base_Types;
with Printable_Object;
package Rectangle is
   subtype Lines_T is Base_Types.Lines_T (1 .. 4);
   type Object_T is new Printable_Object.Object_T with private;
   procedure Set_Lines
     (Object : in out Object_T;
      Lines  :        Lines_T);
   function Lines
     (Object : Object_T)
      return Lines_T;
private
   type Object_T is new Printable_Object.Object_T with record
      Lines : Lines_T;
   end record;
   function Convert
     (Object : Object_T)
      return Base_Types.Lines_T is (Object.Lines);
end Rectangle;
