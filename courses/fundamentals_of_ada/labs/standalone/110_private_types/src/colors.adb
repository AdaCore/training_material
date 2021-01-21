
package body Colors is

   procedure Add
     (Set   : in out Color_Set_T;
      Color :        Color_T) is
   begin
      Set.Values (Color) := True;
   end Add;
   procedure Remove
     (Set   : in out Color_Set_T;
      Color :        Color_T) is
   begin
      Set.Values (Color) := False;
   end Remove;
   function Union
     (Set1, Set2 : Color_Set_T)
      return Color_Set_T is
      Ret_Val : Color_Set_T;
   begin
      for Color in Color_T
      loop
         Ret_Val.Values (Color) :=
           Set1.Values (Color) or else Set2.Values (Color);
      end loop;
      return Ret_Val;
   end Union;

   function Intersection
     (Set1, Set2 : Color_Set_T)
      return Color_Set_T is
      Ret_Val : Color_Set_T;
   begin
      for Color in Color_T
      loop
         Ret_Val.Values (Color) :=
           Set1.Values (Color) and then Set2.Values (Color);
      end loop;
      return Ret_Val;
   end Intersection;

   function Image
     (Set   : Color_Set_T;
      First : Color_T;
      Last  : Color_T)
      return String is
      Str : constant String :=
        (if Set.Values (First) then Color_T'Image (First) else "");
   begin
      if First = Last
      then
         return Str;
      else
         return Str & " " & Image (Set, Color_T'Succ (First), Last);
      end if;
   end Image;

   function Image
     (Set : Color_Set_T)
      return String is
   begin
      return Image (Set, Color_T'First, Color_T'Last);
   end Image;

end Colors;
