package body Basics is

   function Search
     (The_Array :     Arr;
      Val       :     Element;
      At_Index  : out Index)
      return Boolean is
      Pos : Index := The_Array'First;
   begin
      while Pos < The_Array'Last loop
         if The_Array (Pos) = Val then
            At_Index := Pos;
            return True;
         end if;

         Pos := Pos + 1;
      end loop;

      return False;
   end Search;

end Basics;
