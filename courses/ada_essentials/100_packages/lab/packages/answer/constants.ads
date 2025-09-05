package Constants is

   Lowest_Value : constant := 100;
   Highest_Value : constant := 999;
   Maximum_Count : constant := 10;
   subtype Integer_T is Integer range Lowest_Value .. Highest_Value;

end Constants;
