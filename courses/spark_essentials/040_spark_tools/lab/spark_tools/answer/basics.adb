package body Basics with
  SPARK_Mode
is

   procedure Search
     (The_Array :     Arr;
      Val       :     Element;
      At_Index  : out Integer;
      Success   : out Boolean) is
      Pos : Integer := The_Array'First;
   begin
      while Pos in The_Array'Range loop
         if The_Array (Pos) = Val then
            At_Index := Pos;
            Success  := True;
            return;
         end if;
         Pos := Pos + 1;
      end loop;
      Success  := False;
      At_Index := The_Array'Last + 1;
   end Search;

end Basics;
