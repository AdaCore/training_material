package Data_Type is
   type Record_T is null record;  -- Add at least two components

   function ">"
     (L, R : Record_T)
      return Boolean is (True);  --  Update to Return True when L < R

   function Image
     (Element : Record_T)
      return String is
     ("");  -- Update to return string representation of Record_T;

end Data_Type;
