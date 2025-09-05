package body Colors is
   procedure Add (Set : in out Color_Set_T; Color : Color_T) is
   begin
      Set.Values (Color) := True;
   end Add;
   procedure Remove (Set : in out Color_Set_T; Color : Color_T) is
   begin
      Set.Values (Color) := False;
   end Remove;

   function Image
     (Set : Color_Set_T; First : Color_T; Last : Color_T) return String
   is
      Str : constant String :=
        (if Set.Values (First) then Color_T'Image (First) else "");
   begin
      if First = Last then
         return Str;
      else
         return Str & " " & Image (Set, Color_T'Succ (First), Last);
      end if;
   end Image;
   function Image (Set : Color_Set_T) return String
   is (Image (Set, Color_T'First, Color_T'Last));
end Colors;
