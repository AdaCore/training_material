package body Types is

   function Image
     (R : Record_T)
      return String is
   begin
      return R.Left'Image & " " & R.Operator & " " & R.Right'Image;
   end Image;

end Types;
