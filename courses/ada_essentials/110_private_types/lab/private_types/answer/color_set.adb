package body Color_Set is
   function Create (Colors : Color_List_T) return Color_Set_T is
      Ret_Val : Color_Set_T := Empty_Set;
   begin
      for Idx in Colors'Range loop
         Ret_Val.Values (Colors (Idx)) := True;
      end loop;
      return Ret_Val;
   end Create;

   function Contains (Colors : Color_Set_T;
                      Color  : Color_T)
                      return Boolean is
      (Colors.Values (Color));

   function Image (Set   : Color_Set_T;
                   First : Color_T;
                   Last  : Color_T)
                   return String is
      Str : constant String :=
        (if Set.Values (First) then Color_T'Image (First) else "");
   begin
      if First = Last then
         return Str;
      elsif Str'Length = 0 then
         return Image (Set, Color_T'Succ (First), Last);
      else
         return Str & " " & Image (Set, Color_T'Succ (First), Last);
      end if;
   end Image;

   function Image (Set : Color_Set_T) return String is
     (Image (Set, Color_T'First, Color_T'Last));
end Color_Set;
