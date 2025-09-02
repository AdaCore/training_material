with Ada.Text_IO; use Ada.Text_IO;
with Quads;       use Quads;
with Triangles;   use Triangles;
procedure Main2 is
   function Q_Image (S : Quads.Side_T) return String
      renames Quads.Side_T'Image;
   Quad       : constant Quads.Shape_T := (1, 2, 3, 4);
   Quad_Total : Quads.Side_T := 0;

   function T_Image (S : Triangles.Side_T) return String
      renames Triangles.Side_T'Image;
   Triangle       : constant Triangles.Shape_T := (1, 2, 3);
   Triangle_Total : Triangles.Side_T := 0;

begin

   for I in Quad'Range loop
      Quad_Total := Quad_Total + Quad (I);
   end loop;
   Put_Line ("Quad: " & Q_Image (Quad_Total));

   for I in Triangle'Range loop
      Triangle_Total := Triangle_Total + Triangle (I);
   end loop;
   Put_Line ("Triangle: " & T_Image (Triangle_Total));

end Main2;
