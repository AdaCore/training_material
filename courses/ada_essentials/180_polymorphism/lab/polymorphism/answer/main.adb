--Shapes_Body

--Main
with Ada.Text_IO; use Ada.Text_IO;
with Shapes;      use Shapes;
procedure Main is

   Rectangle : constant Shapes.Quadrilateral_T :=
     (Description => "rectangle ",
      Lengths     => (10, 20, 10, 20));
   Triangle : constant Shapes.Triangle_T :=
     (Description => "triangle  ",
      Lengths     => (200, 300, 400));
   Square : constant Shapes.Square_T :=
     (Description => "square    ",
      Lengths     => (5_000, 5_000, 5_000, 5_000));

   procedure Describe (Shape : Shapes.Shape_T'Class) is
   begin
      Put_Line (Shape.Get_Description);
      Put_Line
        ("  Number of sides:" & Integer'Image (Shape.Number_Of_Sides));
      Put_Line ("  Perimeter:" & Shapes.Length_T'Image (Shape.Perimeter));
   end Describe;
begin

   Describe (Rectangle);
   Describe (Triangle);
   Describe (Square);
end Main;
--Main
