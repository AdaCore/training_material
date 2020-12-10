package Swap_Generics is

   generic

      type Data_Type is private;

    procedure Swap_Generic (Value_1 : in out Data_Type; Value_2 : in out Data_Type);

end Swap_Generics;
