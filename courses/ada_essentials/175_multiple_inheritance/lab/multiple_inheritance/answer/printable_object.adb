package body Printable_Object is
   procedure Set_Color (Object : in out Object_T; Color : Base_Types.Color_T)
   is
   begin
      Object.The_Color := Color;
   end Set_Color;
   function Color (Object : Object_T) return Base_Types.Color_T
   is (Object.The_Color);

   procedure Set_Pen_Width
     (Object : in out Object_T; Width : Base_Types.Width_T) is
   begin
      Object.The_Pen_Width := Width;
   end Set_Pen_Width;
   function Pen_Width (Object : Object_T) return Base_Types.Width_T
   is (Object.The_Pen_Width);
end Printable_Object;
