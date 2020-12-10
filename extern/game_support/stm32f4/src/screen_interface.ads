with STM32F4.LCD; use STM32F4.LCD;

package Screen_Interface is
   subtype Width is STM32F4.LCD.Width;
   subtype Height is STM32F4.LCD.Height;

   type Touch_State is record
      Touch_Detected : Boolean;
      X : Width;
      Y : Height;
   end record;

   type Point is record
      X : Width;
      Y : Height;
   end record;

   function "+" (P1, P2 : Point) return Point is (P1.X + P2.X, P1.Y + P2.Y);
   function "-" (P1, P2 : Point) return Point is (P1.X - P2.X, P1.Y - P2.Y);

   subtype Color is STM32F4.LCD.Pixel;

   Black      : Color renames STM32F4.LCD.Black;
   White      : Color renames STM32F4.LCD.White;
   Red        : Color renames STM32F4.LCD.Red;
   Green      : Color renames STM32F4.LCD.Green;
   Blue       : Color renames STM32F4.LCD.Blue;
   Gray       : Color renames STM32F4.LCD.Gray;
   Light_Gray : Color renames STM32F4.LCD.Light_Gray;
   Sky_Blue   : Color renames STM32F4.LCD.Sky_Blue;
   Yellow     : Color renames STM32F4.LCD.Yellow;
   Orange     : Color renames STM32F4.LCD.Orange;
   Pink       : Color renames STM32F4.LCD.Pink;
   Violet     : Color renames STM32F4.LCD.Violet;

   procedure Initialize;
   function Get_Touch_State return Touch_State;
   procedure Set_Pixel (P : Point; Col : Color; Layer : LCD_Layer := Layer1);
   procedure Fill_Screen (Col : Color; Layer : LCD_Layer := Layer1);
   procedure Fast_Horiz_Line (Col : Color; X1: Width; X2 : Width; Y : Height; Layer : LCD_Layer := Layer1);

   type RGB_Value is new Natural range 0 .. 255;
   function RGB_To_Color (R, G, B : RGB_Value) return Color;

end Screen_Interface;
