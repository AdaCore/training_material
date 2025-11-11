package body Color_Set is

   function Create
     (Colors : Color_List_T)
      return Color_Set_T is
      Ret_Val : Color_Set_T;
   begin
      return Ret_Val;
   end Create;

   function Contains
     (Colors : Color_Set_T;
      Color  : Color_T)
      return Boolean is
   begin
      return True;
   end Contains;

   function Image
     (Set   : Color_Set_T;
      First : Color_T;
      Last  : Color_T)
      return String is
   begin
      return "";
   end Image;

   function Image
     (Set : Color_Set_T)
      return String is
   begin
      return "";
   end Image;
end Color_Set;
