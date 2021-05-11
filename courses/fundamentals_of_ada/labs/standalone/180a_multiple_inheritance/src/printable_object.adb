
package body Printable_Object is

   procedure Set_Color
     (Object : in out Object_T;
      Color  :        Base_Types.Color_T) is
   begin
      Object.Color := Color;
   end Set_Color;

   function Color
     (Object : Object_T)
      return Base_Types.Color_T is (Object.Color);

   procedure Set_Pen
     (Object : in out Object_T;
      Size   :        Positive) is
   begin
      Object.Pen_Size := Size;
   end Set_Pen;

   function Pen
     (Object : Object_T)
      return Positive is (Object.Pen_Size);
end Printable_Object;
