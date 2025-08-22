package body Shapes is

   function Perimeter (Lengths : Lengths_T) return Length_T is
      Ret_Val : Length_T := 0;
   begin
      for I in Lengths'First .. Lengths'Last loop
         Ret_Val := Ret_Val + Lengths (I);
      end loop;
      return Ret_Val;
   end Perimeter;

   function Get_Description (Shape : Shape_T'Class) return Description_T is
   begin
      return Shape.Description;
   end Get_Description;

   function Number_Of_Sides (Shape : Quadrilateral_T) return Natural is
   begin
      return 4;
   end Number_Of_Sides;
   function Perimeter (Shape : Quadrilateral_T) return Length_T is
   begin
      return Perimeter (Shape.Lengths);
   end Perimeter;

   function Perimeter (Shape : Square_T) return Length_T is
   begin
      return 4 * Shape.Lengths (Shape.Lengths'First);
   end Perimeter;

   function Number_Of_Sides (Shape : Triangle_T) return Natural is
   begin
      return 3;
   end Number_Of_Sides;
   function Perimeter (Shape : Triangle_T) return Length_T is
   begin
      return Perimeter (Shape.Lengths);
   end Perimeter;
end Shapes;
