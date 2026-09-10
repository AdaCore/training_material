package Basics with
  SPARK_Mode
is

   subtype Index is Integer range 1 .. 10;
   type Element is new Integer;

   type Arr is array (Index) of Element;

   procedure Search
     (The_Array :     Arr;
      Val       :     Element;
      At_Index  : out Integer;
      Success   : out Boolean) with
     Post =>
      (Success and then At_Index in The_Array'Range
       and then The_Array (At_Index) = Val)
      or else (not Success);
   --  Success is True if The_Array contains value Val, in which case
   --  At_Index will contain the first index with value Val.
   --  Success is False otherwise (and At_Index should be "don't care")
end Basics;
