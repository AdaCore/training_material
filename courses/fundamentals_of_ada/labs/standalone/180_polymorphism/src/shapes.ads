with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Shapes is

  type Float_T is digits 6;
  type Vertex_T is record
    X : Float_T;
    Y : Float_T;
  end record;
  type Vertices_T is array (Positive range <>) of Vertex_T;

  type Shape_T is abstract tagged record
    Description : Unbounded_String;
  end record;
  function Get_Description
   (Shape : Shape_T'Class)
    return String;
  function Number_Of_Sides
   (Shape : Shape_T)
    return Natural is abstract;
  function Perimeter
   (Shape : Shape_T)
    return Float_T is abstract;

  type Quadrilateral_T is new Shape_T with record
    Sides : Vertices_T (1 .. 4);
  end record;
  function Number_Of_Sides
   (Shape : Quadrilateral_T)
    return Natural;
  function Perimeter
   (Shape : Quadrilateral_T)
    return Float_T;

  type Square_T is new Quadrilateral_T with null record;
  function Perimeter
   (Shape : Square_T)
    return Float_T;

  type Triangle_T is new Shape_T with record
    Sides : Vertices_T (1 .. 3);
  end record;
  function Number_Of_Sides
   (Shape : Triangle_T)
    return Natural;
  function Perimeter
   (Shape : Triangle_T)
    return Float_T;

end Shapes;
