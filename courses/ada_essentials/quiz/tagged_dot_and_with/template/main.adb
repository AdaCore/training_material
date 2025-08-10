-- Which statement(s) is (are) valid?

--$ begin question
with Shapes;  -- Defines tagged type Shape, primitive Set_Shape
with Colors;  -- Defines tagged type Color, primitive Set_Color
with Weights; -- Defines tagged type Weight,primitive Set_Weight
use Colors;
use type Weights.Weight;

procedure Main is
   The_Shape  : Shapes.Shape;
   The_Color  : Colors.Color;
   The_Weight : Weights.Weight;
--$ end question
begin
   --$ begin cut
   The_Shape.Set_Shape;
   -- "Distinguished Receiver" always allowed
   --$ end cut
   --$ begin cut
   Set_Shape (The_Shape);
   -- No :ada:`use` of :ada:`Colors` or :ada:`use all type` for :ada:`Color`
   --$ end cut
   --$ begin cut
   Set_Color (The_Color);
   -- :ada:`Set_Color` made visible by :ada:`use Colors`
   --$ end cut
   --$ begin cut
   Set_Color (The_Weight);
   -- :ada:`use type Weights.Weight` only gives visibility to operators; needs to be :ada:`use all type`
   --$ end cut
end Main;
