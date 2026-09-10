package Basics is

   subtype Index is Integer range 1 .. 10;
   type Element is new Integer;

   type Arr is array (Index) of Element;

   function Search
     (The_Array :     Arr;
      Val       :     Element;
      At_Index  : out Integer)
      return Boolean;
      --  Returns True if The_Array contains value Val, in which case
      --  At_Index will contain the first index with value Val.
      --  Returns False otherwise.
end Basics;
