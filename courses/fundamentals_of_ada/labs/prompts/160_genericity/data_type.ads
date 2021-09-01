package Data_Type is

   -- Complete implementation
   type Record_T is null record;

  function "<" (L, R : Record_T) return Boolean;

  function Image (Element : Record_T) return String;

end Data_Type;
