with Ada.Real_Time; use Ada.Real_Time;

package body Solar_System.Graphics is

   procedure Draw_Body (Object : Body_T; Canvas : Canvas_ID) is
      Tail_Color : RGBA_T;
      Dimmer : Color_Component_T := 30;
      function Dim (C : Color_Component_T) return Color_Component_T is
      begin
         if C < Dimmer then
            return 0;
         else
            return C - Dimmer;
         end if;
      end Dim;
   begin
      if Object.Visible then
         Draw_Sphere
           (Canvas   => Canvas,
            Position => (Object.Pos.X, Object.Pos.Y, 0.0),
            Radius   => Object.Radius,
            Color    => Object.Color);
         if Object.With_Tail then
            Tail_Color := Object.Color;
            for I in reverse Tail_T'First .. Tail_T'Last loop
               Draw_Sphere
                 (Canvas   => Canvas,
                  Position => (Object.Tail (I).X, Object.Tail (I).Y, 0.0),
                  Radius   => Object.Radius,
                  Color    => Tail_Color);
               Tail_Color.R := Dim (Tail_Color.R);
               Tail_Color.G := Dim (Tail_Color.G);
               Tail_Color.B := Dim (Tail_Color.B);
            end loop;
         end if;
      end if;
   end Draw_Body;



end Solar_System.Graphics;
