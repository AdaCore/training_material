package body Types is

   function Image
     (R : Record_T)
      return String is
   begin
      return
        Numeric_T'Image (R.Left) & " " & R.Operator & " " &
        Numeric_T'Image (R.Right);
   end Image;

end Types;
