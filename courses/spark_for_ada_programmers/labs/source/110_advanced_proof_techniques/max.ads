package Max
  with SPARK_Mode
is

   Min_Table_Size : constant := 1;
   Max_Table_Size : constant := 100;
   Min_Content    : constant := -200;
   Max_Content    : constant := 200;

   type Index_Range   is range Min_Table_Size .. Max_Table_Size;
   type Content_Range is range Min_Content .. Max_Content;
   type Our_Array     is array (Index_Range) of Content_Range;

   function Arrays_Max (A : in Our_Array) return Index_Range
   with Post => (for all N in Index_Range => A (Arrays_Max'Result) >= A (N));

end Max;
