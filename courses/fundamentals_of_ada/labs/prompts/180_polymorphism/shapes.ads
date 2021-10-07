package Shapes is
   pragma Elaborate_Body;

   type Float_T is digits 6;
   type Vertex_T is record
      X : Float_T;
      Y : Float_T;
   end record;
   type Vertices_T is array (Positive range <>) of Vertex_T;

   -- Create abstract Shape_T with some information
   -- Create primitive subprograms to get/set object description,
   -- number of sides, and perimeter

   -- Create concrete Quadrilateral type which is a shape with 4 sides
   -- Implement primitive subprograms as needed

   -- Create concrete Square type which is a Quadrilateral with all 4 sides
   -- of the same length.
   -- Implement primitive subprograms as needed

   -- Create concrete Triangle type which is a shape with 3 sides
   -- Implement primitive subprograms as needed

end Shapes;
