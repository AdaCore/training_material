procedure Sort
  (V    : in out Pkg_Vectors.Vector; First : Index_Type;
   Last :        Index_Type)
is
   procedure Swap_Object (A, B : Index_Type) is
      Temp : Integer := V (A);
   begin
      V (A) := V (B);
      V (B) := Temp;
   end Swap_Object;

   procedure Sort_Object is new Ada.Containers
     .Generic_Sort
     (Index_Type => Index_Type, Before => "<",
      Swap       => Swap_Object);
begin
   Sort_Object (First, Last);
end Sort;
