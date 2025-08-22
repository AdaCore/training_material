package Triangles is

   Number_Of_Sides : constant Natural := 3;
   type Side_T is range 0 .. 1_000;
   type Shape_T is array (1 .. Number_Of_Sides) of Side_T;

end Triangles;
