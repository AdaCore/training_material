package Linear_Search with
  SPARK_Mode
is

   type Index is range 1 .. 10;
   type Element is new Integer;

   type Arr is array (Index) of Element;

   function Non_SPARK_Search
     (A        :     Arr;
      Val      :     Element;
      At_Index : out Index)
      return Boolean;

   type Search_Result is record
      Found    : Boolean;
      At_Index : Index;
   end record;

   function SPARK_Search
     (A   : Arr;
      Val : Element)
      return Search_Result with
     Pre  => Val >= 0,
     Post =>
      (if not SPARK_Search'Result.Found then
         A (SPARK_Search'Result.At_Index) = Val);

end Linear_Search;
