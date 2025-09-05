package body Data_Type is

   function ">" (L, R : Record_T) return Boolean is
   begin
      if L.Character_Component > R.Character_Component then
         return True;
      elsif L.Character_Component < R.Character_Component then
         return False;
      else
         return L.Integer_Component > R.Integer_Component;
      end if;
   end ">";

   function Image (Component : Record_T) return String is
   begin
      return
        Component.Character_Component
        & " =>"
        & Integer'Image (Component.Integer_Component);
   end Image;

end Data_Type;
