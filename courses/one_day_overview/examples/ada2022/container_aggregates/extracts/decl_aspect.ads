type JSON_Array is private
  with Aggregate => (Empty       => New_JSON_Array,
                     Add_Unnamed => Append);

function New_JSON_Array return JSON_Array;

procedure Append
  (Self  : in out JSON_Array;
   Value : JSON_Value) is null;
