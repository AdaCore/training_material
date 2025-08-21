package Data_Type is
   type Record_T is record
      Integer_Component   : Integer;
      Character_Component : Character;
   end record;

   function ">" (L, R : Record_T) return Boolean;

   function Image (Component : Record_T) return String;

end Data_Type;
