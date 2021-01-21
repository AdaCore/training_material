with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Shapes;                use Shapes;
procedure Main is

  Rectangle : Shapes.Quadrilateral_T :=
   (Description => To_Unbounded_String ("rectangle"),
    Sides       => ((0.0, 10.0), (0.0, 20.0), (1.0, 20.0), (1.0, 10.0)));
  Triangle : Shapes.Triangle_T :=
   (Description => To_Unbounded_String ("triangle"),
    Sides       => ((0.0, 0.0), (0.0, 3.0), (4.0, 0.0)));

  Square : Shapes.Square_T :=
   (Description => To_Unbounded_String ("square"),
    Sides       => ((0.0, 1.0), (0.0, 2.0), (1.0, 2.0), (1.0, 1.0)));

  procedure Describe (Shape : Shapes.Shape_T'Class) is
  begin
    Put_Line (Shape.Get_Description);
    if Shape not in Shapes.Shape_T then
      Put_Line ("  Number of sides:" & Integer'Image (Shape.Number_Of_Sides));
      Put_Line ("  Perimeter:" & Shapes.Float_T'Image (Shape.Perimeter));
    end if;
  end Describe;
begin

  Describe (Rectangle);
  Describe (Triangle);
  Describe (Square);

end Main;
