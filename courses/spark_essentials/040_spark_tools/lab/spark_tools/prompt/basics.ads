package Basics is

   type Element is new Integer;

   type Arr is array (Integer range 1 .. 10) of Element;

   function Search
     (The_Array :     Arr;
      Val       :     Element;
      At_Index  : out Integer)
      return Boolean;
      --  Returns True if The_Array contains value Val, in which case
      --  it also returns in At_Index the first index with value Val.
      --  Returns False otherwise.
end Basics;
