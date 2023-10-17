-- Which statement(s) is(are) valid?

--$ begin question
with Shapes; -- Defines tagged type Shape, with primitive P
with Colors; use Colors; -- Defines tagged type Color, with primitive P
with Weights; -- Defines tagged type Weight, with primitive P
use type Weights.Weight;

procedure Main is
   The_Shape : Shapes.Shape;
   The_Color : Colors.Color;
   The_Weight : Weights.Weight;
--$ end question
begin
   --$ line cut
   The_Shape.P;
   --$ line cut
   P (The_Shape);
   --$ line cut
   P (The_Color);
   --$ begin cut
   P (The_Weight);
   -- :ada:`use type` only gives visibility to operators; needs to be :ada:`use all type`
   --$ end cut
end Main;
