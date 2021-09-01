with Ada.Numerics.Generic_Elementary_Functions;
package body Shapes is
  package Math is new Ada.Numerics.Generic_Elementary_Functions (Float_T);

  function Distance (Vertex1 : Vertex_T;
                     Vertex2 : Vertex_T)
                     return Float_T is
   begin
      return Math.Sqrt ((Vertex1.X - Vertex2.X)**2 + (Vertex1.Y - Vertex2.Y)**2);
   end Distance;

   -- Implement primitives as defined in the spec

end Shapes;
