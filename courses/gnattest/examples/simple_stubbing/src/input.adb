--
--  Copyright (C) 2014, AdaCore
--

with Text_IO;

package body Input is
   function Read_Number return Integer is
      S : String := Text_IO.Get_Line;
   begin
      return Integer'Value (S);
   end Read_Number;
end Input;
