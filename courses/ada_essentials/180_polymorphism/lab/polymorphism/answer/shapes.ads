--Shapes_Spec
package Shapes is
   type Length_T is new Natural;
   type Lengths_T is array (Positive range <>) of Length_T;
   subtype Description_T is String (1 .. 10);

   type Shape_T is abstract tagged record
      Description : Description_T;
   end record;
   function Get_Description (Shape : Shape_T'Class) return Description_T;
   function Number_Of_Sides (Shape : Shape_T) return Natural is abstract;
   function Perimeter (Shape : Shape_T) return Length_T is abstract;

   type Quadrilateral_T is new Shape_T with record
      Lengths : Lengths_T (1 .. 4);
   end record;
   function Number_Of_Sides (Shape : Quadrilateral_T) return Natural;
   function Perimeter (Shape : Quadrilateral_T) return Length_T;

   type Square_T is new Quadrilateral_T with null record;
   function Perimeter (Shape : Square_T) return Length_T;

   type Triangle_T is new Shape_T with record
      Lengths : Lengths_T (1 .. 3);
   end record;
   function Number_Of_Sides (Shape : Triangle_T) return Natural;
   function Perimeter (Shape : Triangle_T) return Length_T;
end Shapes;
