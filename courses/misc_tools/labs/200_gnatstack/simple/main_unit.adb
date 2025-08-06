procedure Main_Unit is
   type Data_Type is array (1 .. 5) of Integer;

   function Inverse
     (Input : Data_Type)
      return Data_Type is
      Result : Data_Type;
   begin
      for Index in Data_Type'Range
      loop
         Result (Index) := Input (Data_Type'Last - (Index - Data_Type'First));
      end loop;

      return Result;
   end Inverse;

   Data   : Data_Type := (1, 2, 3, 4, 5);
   Result : Data_Type;
begin
   Result := Inverse (Data);
end Main_Unit;
