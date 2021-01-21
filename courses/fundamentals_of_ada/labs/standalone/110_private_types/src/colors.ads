package Colors is

   type Color_T is (Red, Yellow, Green, Blue, Black);
   type Color_Set_T is private;

   Empty_Set : constant Color_Set_T;

   procedure Add
     (Set   : in out Color_Set_T;
      Color :        Color_T);
   procedure Remove
     (Set   : in out Color_Set_T;
      Color :        Color_T);
   function Union
     (Set1, Set2 : Color_Set_T)
      return Color_Set_T;
   function Intersection
     (Set1, Set2 : Color_Set_T)
      return Color_Set_T;

   function Image
     (Set : Color_Set_T)
      return String;

private
   type Color_Set_Array_T is array (Color_T) of Boolean;
   type Color_Set_T is record
      Values : Color_Set_Array_T := (others => False);
   end record;

   Empty_Set : constant Color_Set_T := (Values => (others => False));

end Colors;
