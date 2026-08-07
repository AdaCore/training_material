package body Basics with
  SPARK_Mode
is

   procedure Search
     (The_Array :     Arr;
      Val       :     Element;
      At_Index  : out Index;
      Status    : out Boolean) is
      Pos : Index := The_Array'First;
   begin
      while Pos < The_Array'Last loop
         if The_Array (Pos) = Val then
            At_Index := Pos;
            Status   := True;
            return;
         end if;

         Pos := Pos + 1;
      end loop;

      At_Index := Index'First;
      Status   := False;
   end Search;

end Basics;
