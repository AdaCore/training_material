with Ada.Text_IO; use Ada.Text_IO;
with Quads;
with Triangles;
procedure Main1 is

   use type Quads.Side_T;
   Q_Sides : Natural renames Quads.Number_Of_Sides;
   Quad    : Quads.Shape_T := (1, 2, 3, 4);
   Quad_Total : Quads.Side_T := 0;

   use type Triangles.Side_T;
   T_Sides  : Natural renames Triangles.Number_Of_Sides;
   Triangle : Triangles.Shape_T := (1, 2, 3);
   Triangle_Total : Triangles.Side_T := 0;

begin

   for I in 1 .. Q_Sides loop
      Quad_Total := Quad_Total + Quad (I);
   end loop;
   Put_Line ("Quad: " & Quads.Side_T'Image (Quad_Total));

   for I in 1 .. T_Sides loop
      Triangle_Total := Triangle_Total + Triangle (I);
   end loop;
   Put_Line ("Triangle: " & Triangles.Side_T'Image (Triangle_Total));

end Main1;
