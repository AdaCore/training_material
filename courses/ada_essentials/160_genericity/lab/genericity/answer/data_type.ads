package Data_Type is
   type Record_T is record
      Integer_Component   : Integer;
      Character_Component : Character;
   end record;

   function ">"
     (L, R : Record_T)
      return Boolean is
     (if L.Character_Component > R.Character_Component then True
      elsif L.Character_Component < R.Character_Component then False
      else L.Integer_Component > R.Integer_Component);

   function Image
     (Component : Record_T)
      return String is
     (Component.Character_Component & " =>" & Integer'Image (Component.Integer_Component));

end Data_Type;
