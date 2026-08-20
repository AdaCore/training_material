package Basics with
  SPARK_Mode
is

   type Index is range 1 .. 10;
   type Element is new Integer;

   type Arr is array (Index) of Element;

   procedure Search
     (The_Array :     Arr;
      Val       :     Element;
      At_Index  : out Index;
      Status    : out Boolean) with
     Post => (not Status) or else (The_Array (At_Index) = Val);
   --  Returns True if The_Array contains value Val, in which case
   --  it also returns in At_Index the first index with value Val.
   --  Returns False otherwise.
end Basics;
