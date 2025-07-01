package body Colors is
   procedure Add
     (Set   : in out Color_Set_T;
      Color :        Color_T) is
   begin
      null;
   end Add;
   procedure Remove
     (Set   : in out Color_Set_T;
      Color :        Color_T) is
   begin
      null;
   end Remove;

   function Image
     (Set : Color_Set_T)
      return String is
   begin
      return "";
   end Image;

end Colors;
