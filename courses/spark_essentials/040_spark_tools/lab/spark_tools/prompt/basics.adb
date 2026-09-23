package body Basics is

   function Search
     (The_Array :     Arr;
      Val       :     Element;
      At_Index  : out Integer)
      return Boolean is
      Pos : Integer := The_Array'First;
   begin
      while Pos in The_Array'Range loop
         if The_Array (Pos) = Val then
            At_Index := Pos;
            return True;
         end if;

         Pos := Pos + 1;
      end loop;

      return False;
   end Search;

end Basics;
