package body Colors is
   procedure Add (Set   : in out Color_Set_T;
                  Color :        Color_T) is
   begin
      Set.Values (Color) := True;
   end Add;
   procedure Remove (Set   : in out Color_Set_T;
                     Color :        Color_T) is
   begin
      Set.Values (Color) := False;
   end Remove;

   function Image (Set   : Color_Set_T;
                   First : Color_T;
                   Last  : Color_T)
                   return String is
      function Image (Color : Color_T) return String is
      begin
         if Set.Values (Color) then
            return Color_T'Image (Color);
         else
            return "";
         end if;
      end Image;
      Str : constant String := Image (First);
   begin
      if First = Last then
         return Str;
      else
         return Str & " " & Image (Set, Color_T'Succ (First), Last);
      end if;
   end Image;
   function Image (Set : Color_Set_T) return String is
   begin
      return Image (Set, Color_T'First, Color_T'Last);
   end Image;
end Colors;
