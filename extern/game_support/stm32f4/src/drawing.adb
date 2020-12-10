package body Drawing is

   --  http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#Ada
   procedure Line (Start, Stop : Point;
                   Col         : Color;
                   Thickness   : Natural := 1)
   is
      DX     : constant Float := abs Float (Stop.X - Start.X);
      DY     : constant Float := abs Float (Stop.Y - Start.Y);
      Err    : Float;
      X      : Natural        := Start.X;
      Y      : Natural        := Start.Y;
      Step_X : Integer        := 1;
      Step_Y : Integer        := 1;

      procedure Draw_Point (P : Point) is
      begin
         if Thickness /= 1 then
            Line ((P.X - Width (Thickness / 2), P.Y),
                  (P.X + Width (Thickness / 2), P.Y),
                  Col, 1);
            Line ((P.X, P.Y - Height (Thickness / 2)),
                  (P.X, P.Y + Height (Thickness / 2)),
                  Col, 1);
         else
            Set_Pixel ((P.X, P.Y), Col);
         end if;
      end Draw_Point;
      pragma Inline (Draw_Point);

   begin
      if Start.X > Stop.X then
         Step_X := -1;
      end if;

      if Start.Y > Stop.Y then
         Step_Y := -1;
      end if;

      if DX > DY then
         Err := DX / 2.0;
         while X /= Stop.X loop
            Draw_Point ((X, Y));
            Err := Err - DY;
            if Err < 0.0 then
               Y := Y + Step_Y;
               Err := Err + DX;
            end if;
            X := X + Step_X;
         end loop;
      else
         Err := DY / 2.0;
         while Y /= Stop.Y loop
            Draw_Point ((X, Y));
            Err := Err - DX;
            if Err < 0.0 then
               X := X + Step_X;
               Err := Err + DY;
            end if;
            Y := Y + Step_Y;
         end loop;
      end if;

      Draw_Point ((X, Y));
   end Line;

   procedure Rect (Start, Stop : Point; Col : Color;
                   Thickness   : Natural := 1) is
   begin
      Drawing.Line (Start, (Stop.X, Start.Y), Col, Thickness);
      Drawing.Line ((Stop.X, Start.Y), Stop, Col, Thickness);
      Drawing.Line (Stop, (Start.X, Stop.Y), Col, Thickness);
      Drawing.Line ((Start.X, Stop.Y), Start, Col, Thickness);
   end Rect;

   procedure Rect_Fill (Start, Stop : Point; Col : Color) is
      P1 :  Point := Start;
      P2 : Point := (Start.X, Stop.Y);
   begin
      loop
         Line (P2, P1, Col);
         exit when P2.X = Stop.X;
         P1.X := P1.X + 1;
         P2.X := P2.X + 1;
      end loop;
   end Rect_Fill;

   --  http://rosettacode.org/wiki/Bitmap/B%C3%A9zier_curves/Cubic
   procedure Cubic_Bezier (P1, P2, P3, P4 : Point;
                           Col            : Color;
                           N              : Positive := 20;
                           Thickness      : Natural := 1)
   is
      Points : array (0 .. N) of Point;
   begin
      for I in Points'Range loop
         declare
            T : constant Float := Float (I) / Float (N);
            A : constant Float := (1.0 - T)**3;
            B : constant Float := 3.0 * T * (1.0 - T)**2;
            C : constant Float := 3.0 * T**2 * (1.0 - T);
            D : constant Float := T**3;
         begin
            Points (I).X := Width  (A * Float (P1.X) +
                                    B * Float (P2.X) +
                                    C * Float (P3.X) +
                                    D * Float (P4.X));
            Points (I).Y := Height (A * Float (P1.Y) +
                                    B * Float (P2.Y) +
                                    C * Float (P3.Y) +
                                    D * Float (P4.Y));
         end;
      end loop;
      for I in Points'First .. Points'Last - 1 loop
         Line (Points (I), Points (I + 1), Col, Thickness => Thickness);
      end loop;
   end Cubic_Bezier;

   --  http://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm
   procedure Circle (Center : Point;
                     Radius : Natural;
                     Col    : Color;
                     Fill   : Boolean := False)
   is
      F     : Integer := 1 - Radius;
      ddF_X : Integer := 0;
      ddF_Y : Integer := (-2) * Radius;
      X     : Integer := 0;
      Y     : Integer := Radius;
   begin
      if Fill then
         for Cnt in 1 .. Radius loop
            Circle (Center, Cnt, Col, False);
         end loop;
         return;
      end if;

      Set_Pixel ((Center.X, Center.Y + Radius), Col);
      Set_Pixel ((Center.X, Center.Y - Radius), Col);
      Set_Pixel ((Center.X + Radius, Center.Y), Col);
      Set_Pixel ((Center.X - Radius, Center.Y), Col);
      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;
         Set_Pixel ((Center.X + X, Center.Y + Y), Col);
         Set_Pixel ((Center.X - X, Center.Y + Y), Col);
         Set_Pixel ((Center.X + X, Center.Y - Y), Col);
         Set_Pixel ((Center.X - X, Center.Y - Y), Col);
         Set_Pixel ((Center.X + Y, Center.Y + X), Col);
         Set_Pixel ((Center.X - Y, Center.Y + X), Col);
         Set_Pixel ((Center.X + Y, Center.Y - X), Col);
         Set_Pixel ((Center.X - Y, Center.Y - X), Col);
      end loop;
   end Circle;
end Drawing;
