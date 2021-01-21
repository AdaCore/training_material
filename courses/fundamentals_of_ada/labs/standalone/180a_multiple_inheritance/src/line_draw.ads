
with Base_Types;
package Line_Draw is

   type Object_T is interface;
   procedure Set_Color
     (Object : in out Object_T;
      Color  :        Base_Types.Color_T) is abstract;
   function Color
     (Object : Object_T)
      return Base_Types.Color_T is abstract;
   procedure Set_Pen
     (Object : in out Object_T;
      Size   :        Positive) is abstract;
   function Pen
     (Object : Object_T)
      return Positive is abstract;
   function Convert
     (Object : Object_T)
      return Base_Types.Lines_T is abstract;
   procedure Print (Object : Object_T'Class);

end Line_Draw;
