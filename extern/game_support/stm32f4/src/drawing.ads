with Screen_interface; use Screen_Interface;

package Drawing is

   procedure Line (Start, Stop : Point;
                   Col         : Color;
                   Thickness   : Natural := 1);
   procedure Rect (Start, Stop : Point; Col : Color;
                   Thickness   : Natural := 1);
   procedure Rect_Fill (Start, Stop : Point; Col : Color);
   procedure Cubic_Bezier (P1, P2, P3, P4 : Point;
                           Col            : Color;
                           N              : Positive := 20;
                           Thickness      : Natural := 1);
   procedure Circle (Center : Point;
                     Radius : Natural;
                     Col    : Color;
                     Fill   : Boolean := False);

end Drawing;
