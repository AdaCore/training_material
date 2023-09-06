package Sorts is

   type Integer_List  is array (Integer range <>) of Integer;

   procedure Sort(List : in out Integer_List);

   procedure Display_List (List : in Integer_List);

end Sorts;
