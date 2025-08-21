--Shapes_Spec

--Shapes_Body
package body Shapes is

   function Perimeter (Lengths : Lengths_T) return Length_T is
      Ret_Val : Length_T := 0;
   begin
      for I in Lengths'First .. Lengths'Last
      loop
         Ret_Val := Ret_Val + Lengths (I);
      end loop;
      return Ret_Val;
   end Perimeter;

   function Get_Description (Shape : Shape_T'Class) return Description_T is
      (Shape.Description);

   function Number_Of_Sides (Shape : Quadrilateral_T) return Natural is
      (4);
   function Perimeter (Shape : Quadrilateral_T) return Length_T is
      (Perimeter (Shape.Lengths));

   function Perimeter (Shape : Square_T) return Length_T is
      (4 * Shape.Lengths (Shape.Lengths'First));

   function Number_Of_Sides (Shape : Triangle_T) return Natural is
      (3);
   function Perimeter (Shape : Triangle_T) return Length_T is
      (Perimeter (Shape.Lengths));
end Shapes;
