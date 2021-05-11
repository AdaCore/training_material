with Ada.Numerics.Generic_Elementary_Functions;
package body Shapes is

  package Math is new Ada.Numerics.Generic_Elementary_Functions (Float_T);

  function Distance
   (Vertex1 : Vertex_T;
    Vertex2 : Vertex_T)
    return Float_T is
   (Math.Sqrt ((Vertex1.X - Vertex2.X)**2 + (Vertex1.Y - Vertex2.Y)**2));
  function Perimeter
   (Vertices : Vertices_T)
    return Float_T is
    Ret_Val : Float_T := 0.0;
  begin
    for I in Vertices'First .. Vertices'Last - 1 loop
      Ret_Val := Ret_Val + Distance (Vertices (I), Vertices (I + 1));
    end loop;
    Ret_Val :=
     Ret_Val + Distance (Vertices (Vertices'Last), Vertices (Vertices'First));
    return Ret_Val;
  end Perimeter;

  function Get_Description
   (Shape : Shape_T'Class)
    return String is (To_String (Shape.Description));

  function Number_Of_Sides
   (Shape : Quadrilateral_T)
    return Natural is (4);
  function Perimeter
   (Shape : Quadrilateral_T)
    return Float_T is (Perimeter (Shape.Sides));

  function Perimeter
   (Shape : Square_T)
    return Float_T is (4.0 * Distance (Shape.Sides (1), Shape.Sides (2)));

  function Number_Of_Sides
   (Shape : Triangle_T)
    return Natural is (3);
  function Perimeter
   (Shape : Triangle_T)
    return Float_T is (Perimeter (Shape.Sides));

end Shapes;
