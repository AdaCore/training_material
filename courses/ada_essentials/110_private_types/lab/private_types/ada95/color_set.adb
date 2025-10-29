package body Color_Set is

   function Create
     (Colors : Color_List_T)
      return Color_Set_T is
      Ret_Val : Color_Set_T := Empty_Set;
   begin
      for Idx in Colors'Range loop
         Ret_Val.Values (Colors (Idx)) := True;
      end loop;
      return Ret_Val;
   end Create;

   function Contains
     (Colors : Color_Set_T;
      Color  : Color_T)
      return Boolean is
   begin
      return Colors.Values (Color);
   end Contains;

   function Image
     (Set   : Color_Set_T;
      First : Color_T;
      Last  : Color_T)
      return String is
   begin
      if First = Last then
         if Set.Values (First) then
            return Color_T'Image (First);
         else
            return "";
         end if;
      else
         declare
            Recursed : constant String :=
              Image (Set, Color_T'Succ (First), Last);
         begin
            if Set.Values (First) then
               return Color_T'Image (First) & " " & Recursed;
            else
               return Recursed;
            end if;
         end;
      end if;
   end Image;

   function Image
     (Set : Color_Set_T)
      return String is
   begin
      return Image (Set, Color_T'First, Color_T'Last);
   end Image;
end Color_Set;
