--
--  Copyright (C) 2014, AdaCore
--

with Input;

package body Square is

   function Square return Integer is
      A : Integer := Input.Read_Number;
   begin
      return A * A;
   end Square;

end Square;
