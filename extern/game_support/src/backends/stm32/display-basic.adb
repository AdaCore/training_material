-----------------------------------------------------------------------
--                              Ada Labs                             --
--                                                                   --
--                 Copyright (C) 2008-2013, AdaCore                  --
--                                                                   --
-- Labs is free  software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Interfaces.C; use Interfaces.C;
with System;
with Ada.Text_IO; use Ada.Text_IO;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;
with Interfaces; use Interfaces;
with Screen_Interface;
with STM32F4.LCD; use STM32F4;
with Fonts; use Fonts;
with Ada.Unchecked_Deallocation;

package body Display.Basic is

   use LCD;

   function RGBA_To_ARGB1555(Color : RGBA_T) return STM32F4.LCD.Pixel_Color;

   function RGBA_To_ARGB1555(Color : RGBA_T) return STM32F4.LCD.Pixel_Color is
      Output : STM32F4.LCD.Pixel_Color;
   begin
      Output.A := Bits_1(Color.A / 128);
      Output.R := Bits_5(Color.R / 8);
      Output.G := Bits_5(Color.G / 8);
      Output.B := Bits_5(Color.B / 8);
      return Output;
   end RGBA_To_ARGB1555;

   function RGBA_To_16(Color : RGBA_T) return STM32F4.LCD.Pixel is
   begin
      return Pixel_Color_To_Pixel(RGBA_To_ARGB1555(Color));
   end RGBA_To_16;


   function Switch_Layer (L : LCD.LCD_Layer) return LCD.LCD_Layer
   is
     (if L = Layer1 then Layer2 else Layer1);


   type T_Internal_Canvas is record
      Zoom_Factor : Float := 1.0;
      width : Integer := 240;
      Height : Integer := 320;
       Center : Screen_Point := (0, 0);
   end record;

   Internal_Canvas : T_Internal_Canvas;

   package Sc renames Screen_Interface;
   subtype Sc_Color is Screen_Interface.Color;

   procedure Set_Pixel (X, Y : Integer; Col : SC.Color) is
   begin
      if X in SC.Width'Range and then Y in SC.Height'Range then
         SC.Set_Pixel ((X, Y), Col);
      end if;
   end Set_Pixel;

   procedure Set_Pixel (Canvas : Canvas_ID; Position : Screen_Point; Color : RGBA_T) is
   begin
      Set_Pixel(Position.X, Position.Y, RGBA_To_16(Color));
   end Set_Pixel;


   use Screen_Interface;

   function Scale (C : T_Internal_Canvas; L : Float) return Integer is (Integer (L * C.Zoom_Factor));

   type Cart_Point is record
      X, Y : Float;
   end record;


   --     function "+" (P : Cart_Point) return Screen_Point
   --     is
   --       ((X => Integer (Zoom_Factor * P.X) + SC.Width'Last / 2,
   --         Y => SC.Height'Last / 2 - Integer (Zoom_Factor * P.Y)));


   function To_Screen_Point (C : T_Internal_Canvas; P : Cart_Point) return Screen_Point
   is
     ((X => Integer (C.Zoom_Factor * P.X) + Integer(C.width / 2) - C.Center.X,
       Y => Integer(C.Height / 2) + C.Center.Y - Integer (C.Zoom_Factor * P.Y)));


   function To_Point3d (Canvas : Canvas_ID; P : Screen_Point) return Point_3d is

   begin
      return (Float(P.X - Integer(Internal_Canvas.width / 2)),
              Float(Integer(Internal_Canvas.Height / 2)- P.Y),
              0.0);
   end To_Point3d;

   function To_Screen_Point (Canvas : Canvas_ID;  P : Point_3d) return Screen_Point is
   begin
      return To_Screen_Point(Internal_Canvas, (P.X, P.Y));
   end To_Screen_Point;

   procedure Set_Center (Canvas : Canvas_ID; Position : Screen_Point) is
   begin
      Internal_Canvas.Center := Position;
   end Set_Center;

   procedure Set_Center (Canvas : Canvas_ID; Position : Point_3d) is
    begin
      Set_Center(Canvas,
                 Screen_Point'(Scale(Internal_Canvas, Position.X),
                   Scale(Internal_Canvas, Position.Y)));
   end Set_Center;

   function Get_Center (Canvas : Canvas_ID) return Screen_Point is
   begin
      return Internal_Canvas.Center;
   end Get_Center;

   function Get_Canvas_Size(Canvas : Canvas_ID) return Screen_Point is
   begin
      return Screen_Point'(Internal_Canvas.width, Internal_Canvas.Height);
   end Get_Canvas_Size;

   ------------------------------
   -- SHAPE RECORD AND STORAGE --
   ------------------------------

   type Shape_Kind is (Circle, Line, Torus, Box, Text, No_Shape);

   type String_Access is access all String;

   Max_String_Length : constant := 255;

   type Managed_String is record
      Str : String (1 .. 255);
      Length : Natural;
   end record;

   Nb_Max_Strings : constant := 64;

   type String_Idx is range 1 .. Nb_Max_Strings;
   type String_Array is array (String_Idx) of Managed_String;
   type Managed_Strings is record
      Strings : String_Array;
      Current_Idx : String_Idx := 1;
   end record;

   String_Store : Managed_Strings;

--     type Shape (Kind : Shape_Kind := No_Shape) is record
--        X, Y : Float;
--        Color : Color_Type;
--        case Kind is
--           when Circle   => Radius : Float;
--           when Line     => End_X, End_Y : Float;
--           when Torus    => Outer, Inner : Float;
--           when Box      => Width, Height : Float;
--              --  when Dyn_Text => Str_Ptr           : String_Access;
--           when Text      => Str_Idx          : String_Idx;
--           when No_Shape => null;
--        end Case;
--     end record;



--     Color_Map : array (Color_Type) of Sc_Color :=
--       (Black   => Sc.Black,
--        Blue    => Sc.Blue,
--        Green   => Sc.Green,
--        Cyan    => Sc.Pink,
--        Red     => Sc.Red,
--        Magenta => Sc.Orange,
--        Yellow  => Sc.Yellow,
--        White   => Sc.White,
--        Gray    => Sc.Gray);

--     subtype Shape_Index is Shape_Id range 1 .. Max_Shapes;
--
--     type Shapes_Array is array (Shape_Index) of Shape;
--     Shapes : Shapes_Array :=
--       (others => (Kind => No_Shape, others => <>));
--     Max_Shape_Id : Shape_Index := 1;

   -----------
   -- Set_X --
   -----------

--     procedure Set_X (Shape : in out Shape_Id; Value : Float) is
--     begin
--        Shapes (Shape).X := Value;
--     end Set_X;

   -----------
   -- Get_X --
   -----------

--     function Get_X (Shape : Shape_Id) return Float is
--     begin
--        return Shapes (Shape).X;
--     end Get_X;

   -----------
   -- Set_Y --
   -----------

--     procedure Set_Y (Shape : in out Shape_Id; Value : Float) is
--     begin
--        Shapes (Shape).Y := Value;
--     end Set_Y;

   -----------
   -- Get_Y --
   -----------

--     function Get_Y (Shape : Shape_Id) return Float is
--     begin
--        return Shapes (Shape).Y;
--     end Get_Y;

   ---------------
   -- Set_Color --
   ---------------

--     procedure Set_Color (Shape : in out Shape_Id; Color : Color_Type) is
--     begin
--        Shapes (Shape).Color := Color;
--     end Set_Color;

   ---------------
   -- Get_Color --
   ---------------

--     function Get_Color (Shape : Shape_Id) return Color_Type is
--     begin
--        return Shapes (Shape).Color;
--     end Get_Color;

   ------------
   -- Delete --
   ------------

--     procedure Delete (Shape : in out Shape_Id) is
--     begin
--        Shapes (Shape) := (Kind => No_Shape, others => <>);
--     end Delete;

   ---------------
   -- New_Shape --
   ---------------

--     function New_Shape return Shape_Id is
--     begin
--        for I in Shapes'Range loop
--           if Shapes (I).Kind = No_Shape then
--              if I > Max_Shape_Id then
--                 Max_Shape_Id := I;
--              end if;
--              return I;
--           end if;
--        end loop;
--
--        return Null_Shape_Id;
--     end New_Shape;

   ----------------
   -- New_Circle --
   ----------------

--     function New_Circle
--       (X      : Float;
--        Y      : Float;
--        Radius : Float;
--        Color  : Color_Type)
--        return Shape_Id
--     is
--        Id : Shape_Index := New_Shape;
--     begin
--        Shapes (Id) := (Kind => Circle, X => X, Y => Y, Radius => Radius,
--                        Color => Color);
--        return Id;
--     end New_Circle;

   ----------------
   -- Set_Radius --
   ----------------

--     procedure Set_Radius (Shape : in out Shape_Id; Value : Float) is
--     begin
--        Shapes (Shape).Radius := Value;
--     end Set_Radius;

   ----------------
   -- Get_Radius --
   ----------------

--     function Get_Radius (Shape : Shape_Id) return Float is
--     begin
--        return Shapes (Shape).Radius;
--     end Get_Radius;

   --------------
   -- New_Line --
   --------------

--     function New_Line
--       (X      : Float;
--        Y      : Float;
--        X2     : Float;
--        Y2     : Float;
--        Color  : Color_Type) return Shape_Id
--     is
--        Id : Shape_Index := New_Shape;
--     begin
--        Shapes (Id) := (Kind => Line, X => X, Y => Y, End_X => X2, End_Y => Y2,
--                        Color => Color);
--        return Id;
--     end New_Line;

   ------------------
   -- New_Dyn_Text --
   ------------------

   --     function New_Dyn_Text
   --       (X      : Float;
   --        Y      : Float;
   --        Str   : String;
   --        Color  : Color_Type) return Shape_Id
   --     is
   --        Id : Shape_Index := New_Shape;
   --        New_Str : String_Access := new String'(Str);
   --     begin
   --        Shapes (Id) := (Kind => Dyn_Text, X => X, Y => Y,
   --                        Str_Ptr => New_Str,
   --                        Color => Color);
   --        return Id;
   --     end New_Dyn_Text;


   function New_Str (Str : String) return String_Idx is
      C : String_Idx := String_Store.Current_Idx;
   begin

      String_Store.Strings (C).Str (1 .. Str'Length) := Str;
      String_Store.Strings(C).Length := Str'Length;
      String_Store.Current_Idx := String_Store.Current_Idx + 1;

      return C;
   end New_Str;

   procedure Set_Str (Str_Idx : String_Idx; Str : string) is
   begin
      String_Store.Strings (Str_Idx).Str (1 .. Str'Length) := Str;
      String_Store.Strings(Str_Idx).Length := Str'Length;

   end Set_Str;

   function Get_Str (Str_Idx : String_Idx) return String is
      Str : String (1 .. String_Store.Strings (Str_Idx).Length) :=
        String_Store.Strings (Str_Idx).Str (1 .. String_Store.Strings (Str_Idx).Length);
   begin
      return Str;
   end Get_Str;

   --------------
   -- New_Text --
   --------------

--     function New_Text
--       (X      : Float;
--        Y      : Float;
--        Text   : String;
--        Color  : Color_Type) return Shape_Id
--     is
--        Id : Shape_Index := New_Shape;
--     begin
--        Shapes (Id) := (Kind => Display.Basic.Text, X => X, Y => Y,
--                        Str_Idx => New_Str (Text),
--                        Color => Color);
--        return Id;
--     end New_Text;


   --     procedure Set_Dyn_Text (V : in out Shape_Id; Text : String) is
   --        procedure Free_String is new Ada.Unchecked_Deallocation (String, String_Access);
   --
   --     begin
   --
   --        Free_String (Shapes (V).Str_Ptr);
   --        Shapes(V).Str_Ptr := new String'(Text);
   --     end Set_Dyn_Text;
   --
--     procedure Set_Text (V : in out Shape_Id; Text : String) is
--     begin
--        Set_Str (Shapes(V).Str_Idx, Text);
--     end Set_Text;

--     function Get_Text (V : Shape_Id) return String is
--        Str : String (1 .. String_Store.Strings (Shapes (V).Str_Idx).Length) := String_Store.Strings (Shapes (V).Str_Idx).Str (1 .. String_Store.Strings (Shapes (V).Str_Idx).Length);
--     begin
--        return Str;
--     end Get_Text;

   --     function Get_Dyn_Text (V : Shape_Id) return String is
   --     begin
   --        return Shapes(V).Str_Ptr.all;
   --     end Get_Dyn_Text;


   ------------
   -- Get_X2 --
   ------------

--     function Get_X2 (V : Shape_Id) return Float is
--     begin
--        return Shapes (V).End_X;
--     end Get_X2;

   ------------
   -- Get_Y2 --
   ------------

--     function Get_Y2 (V : Shape_Id) return Float is
--     begin
--        return Shapes (V).End_Y;
--     end Get_Y2;

   ------------
   -- Set_X2 --
   ------------

--     procedure Set_X2 (V : in out Shape_Id; Value : Float) is
--     begin
--        Shapes (V).End_X := Value;
--     end Set_X2;

   ------------
   -- Set_Y2 --
   ------------

--     procedure Set_Y2 (V : in out Shape_Id; Value : Float)
--     is
--     begin
--        Shapes (V).End_Y := Value;
--     end Set_Y2;

   -------------
   -- New_Box --
   -------------

--     function New_Box
--       (X,  Y          : Float;
--        Width, Height : Float;
--        Color  : Color_Type)
--        return Shape_Id
--     is
--        Id : Shape_Index := New_Shape;
--     begin
--        Shapes (Id) := (Kind => Box, X => X, Y => Y, Width => Width,
--                        Height => Height, Color => Color);
--        return Id;
--     end New_Box;

   ---------------
   -- New_Torus --
   ---------------

--     function New_Torus
--       (X            : Float;
--        Y            : Float;
--        Inner_Radius : Float;
--        Outer_Radius : Float;
--        Color        : Color_Type)
--        return Shape_Id
--     is
--        Id : Shape_Index := New_Shape;
--     begin
--        Shapes (Id) := (Kind => Torus, X => X, Y => Y,
--                        Inner => Outer_Radius - Inner_Radius,
--                        Outer => Outer_Radius,
--                        Color => Color);
--        return Id;
--     end New_Torus;

   ----------------------
   -- Set_Inner_Radius --
   ----------------------
--
--     procedure Set_Inner_Radius (V : in out Shape_Id; Value : Float) is
--     begin
--        Shapes (V).Inner := Value;
--     end Set_Inner_Radius;

   ----------------------
   -- Set_Outer_Radius --
   ----------------------

--     procedure Set_Outer_Radius (V : in out Shape_Id; Value : Float) is
--     begin
--        Shapes (V).Outer := Value;
--     end Set_Outer_Radius;

   ----------------------
   -- Get_Inner_Radius --
   ----------------------

--     function Get_Inner_Radius (V : Shape_Id) return Float is
--     begin
--        return Shapes (V).Inner;
--     end Get_Inner_Radius;

   ----------------------
   -- Get_Outer_Radius --
   ----------------------

--     function Get_Outer_Radius (V : Shape_Id) return Float is
--     begin
--        return Shapes (V).Outer;
--     end Get_Outer_Radius;

   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle (P : Screen_Point; Radius : Integer; Color : Sc_Color)
   is
      CX, CY, Radius_Error : Integer;
   begin
      CX := Radius;
      CY := 0;
      Radius_Error := 1 - CX;

      while CX >= CY loop
         Set_Pixel ( CX + P.X,  CY + P.Y, Color);
         Set_Pixel ( CY + P.X,  CX + P.Y, Color);
         Set_Pixel (-CX + P.X,  CY + P.Y, Color);
         Set_Pixel (-CY + P.X,  CX + P.Y, Color);
         Set_Pixel (-CX + P.X, -CY + P.Y, Color);
         Set_Pixel (-CY + P.X, -CX + P.Y, Color);
         Set_Pixel ( CX + P.X, -CY + P.Y, Color);
         Set_Pixel ( CY + P.X, -CX + P.Y, Color);
         CY := CY + 1;
         if Radius_Error < 0 then
            Radius_Error := Radius_Error + (2 * CY + 1);
         else
            CX := CX - 1;
            Radius_Error := Radius_Error + (2 * (CY - CX) + 1);
         end if;
      end loop;

   end Draw_Circle;

   procedure Draw_Circle (Canvas : Canvas_ID; Position: Point_3d; Radius : Float; Color: RGBA_T) is
   begin
      Draw_Circle(To_Screen_Point(Internal_Canvas, (Position.X, Position.Y)),
                  Scale (Internal_Canvas, Radius),
                  RGBA_To_16(Color));
   end Draw_Circle;

   procedure Draw_Circle (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T) is
    begin
      Draw_Circle(Position,
                  Radius,
                  RGBA_To_16(Color));
   end Draw_Circle;


    ---------------
   -- Draw_Line --
   ---------------

   --     procedure Draw_Line
   --       (P_Start, P_End : Screen_Point; Color : Sc_Color)
   --     is
   --        DX : Integer := P_End.X - P_Start.X;
   --        DY : Integer := P_End.Y - P_Start.Y;
   --        D  : Integer := 2 * DY - DX;
   --        Y  : Integer := P_Start.Y;
   --     begin
   --        Set_Pixel ((P_Start.X, P_Start.Y), Color);
   --        for X in P_Start.X +1 .. P_End.X loop
   --           if D > 0 then
   --              Y := Y + 1;
   --              Set_Pixel ((X, Y), Color);
   --              D := D + (2 * DY - 2 * DX);
   --           else
   --              Set_Pixel ((X, Y), Color);
   --              D := D + (2 * DY);
   --           end if;
   --        end loop;
   --     end Draw_Line;


   procedure Draw_Line (P0 : Screen_Point; P1 : Screen_Point; Pixel : Sc_Color) is
      dx                 : constant Integer := abs (P1.X - P0.X);
      sx                 : constant Integer := (if P0.X < P1.X then 1 else -1);
      dy                 : constant Integer := abs (P1.Y - P0.Y);
      sy                 : constant Integer := (if P0.Y < P1.Y then 1 else -1);
      err                : Integer := (if dx > dy then dx else - dy) / 2;
      e2                 : Integer;
      X                  : Integer := P0.X;
      Y                  : Integer := P0.Y;

   begin
      loop
         Set_Pixel (X, Y, Pixel);
         if X = P1.X and then Y = P1.Y then
            return;
         end if;
         e2 := err;
         if e2 > -dx then
            err := err - dy;
            X  := X + sx;
         end if;
         if e2 < dy then
            err := err + dx;
            Y := Y + sy;
         end if;
      end loop;
   end Draw_Line;


   procedure Draw_Line (Canvas : Canvas_ID; P1: Point_3d; P2 : Point_3d; Color: RGBA_T) is
   begin
      Draw_Line (To_Screen_Point(Internal_Canvas, (P1.X, P1.Y)),
                 To_Screen_Point(Internal_Canvas, (P2.X, P2.Y)),
                 RGBA_To_16(Color));

   end Draw_Line;

   procedure Draw_Line (Canvas : Canvas_ID; P1: Screen_Point; P2 : Screen_Point; Color: RGBA_T) is
   begin
      Draw_Line(P1, P2, RGBA_To_16(Color));
   end Draw_Line;


   --------------
   -- Draw_Box --
   --------------

   procedure Draw_Box
     (P : Screen_Point; Width, Height : Integer; Color : Sc_Color)
   is
      X1 : Integer := P.X;
      X2 : Integer := P.X + Width;

   begin
      if X1 < 0 then
         X1 := 0;
      end if;
      if X2 > Internal_Canvas.width - 1 then
         X2 := Internal_Canvas.width -1;
      end if;

      for Y in 0 .. Height - 1  loop
         if P.Y + Y >= 0 and then P.Y + Y < Internal_Canvas.Height then
            Fast_Horiz_Line (Color, X1, X2, P.Y + Y);
         end if;
      end loop;
   end Draw_Box;

   procedure Draw_Rect
     (P : Screen_Point; Width, Height : Integer; Color : Sc_Color)
   is
   begin
      Draw_Line(P, (P.x + Width -1, P.Y), Color);
      Draw_Line(P, (P.x, P.Y + Height - 1), Color);
      Draw_Line((P.x + Width - 1, P.Y), (P.x + Width - 1, P.Y + Height - 1), Color);
      Draw_Line((P.x, P.Y + Height - 1), (P.x + Width - 1, P.Y + Height - 1), Color);
   end Draw_Rect;

   procedure Draw_Rect (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T) is
   begin
      Draw_Rect(Position, Width, Height, RGBA_To_16(Color));
   end Draw_Rect;

   procedure Draw_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T) is
   begin
      Draw_Rect(To_Screen_Point(Internal_Canvas, (Position.X, Position.Y)), Scale(Internal_Canvas, Width), Scale(Internal_Canvas, Height), RGBA_To_16(Color));
   end Draw_Rect;


   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T) is
   begin
      Draw_Box(Position, Width, Height, RGBA_To_16(Color));
   end Draw_Fill_Rect;

   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T) is
   begin
      Draw_Box(To_Screen_Point(Internal_Canvas, (Position.X, Position.Y)), Scale(Internal_Canvas, Width), Scale(Internal_Canvas, Height), RGBA_To_16(Color));
   end Draw_Fill_Rect;







   ------------------------
   -- Draw_Filled_Circle --
   ------------------------

   --   procedure Draw_Filled_Circle
   --    (P : Screen_Point; Radius : Integer; Color : Sc_Color)
   --   is
   --   begin
   --      for Y in -Radius .. Radius loop
   --         for X in -Radius .. Radius loop
   --            if X * X + Y * Y < Radius * Radius then
   --               Set_Pixel (P.X + X, P.Y + Y, Color);
   --            end if;
   --         end loop;
   --      end loop;
   --   end Draw_Filled_Circle;
   procedure Draw_Filled_Circle (P : Screen_Point; Radius : Integer; Color : Sc_Color) is
      r                  : Integer := Radius;
      x                  : Integer := -r;
      y                  : Integer := 0;
      err                : Integer := 2 - 2 * r; --/  * II. Quadrant *  /

   begin
      if Radius <= 1 then
         Set_Pixel (P.X, P.Y, Color);

      else

         while x < 0 loop

            Draw_Line ((P.X - x, P.Y - y), (P.X + x, P.Y - y), Color);
            Draw_Line ((P.X - x, P.Y + y), (P.X + x, P.Y + y), Color);

            r := err;
            if r <= y then
               y := y + 1;
               err := err +  y * 2 + 1; --           /  * e_xy + e_y < 0 *  /
            end if;
            if r > x or else  err > y then
               x := x + 1;
               err := err + x * 2 + 1; --/  * e_xy + e_x > 0 or no 2nd y - step *  /
            end if;
            if x >= 0 then
               return;
            end if;
         end loop;
      end if;

   end Draw_Filled_Circle;



   procedure Draw_Sphere (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T) is
   begin
      Draw_Filled_Circle (Position, Radius, RGBA_To_16(Color));
   end Draw_Sphere;

   procedure Draw_Sphere (Canvas : Canvas_ID; Position: Point_3d; Radius : Float; Color: RGBA_T) is
   begin
      Draw_Filled_Circle (To_Screen_Point(Internal_Canvas, (Position.X, Position.Y)),
                          Scale(Internal_Canvas, Radius),
                          RGBA_To_16(Color));
   end Draw_Sphere;


   ---------------
   -- Draw_Ring --
   ---------------

   procedure Draw_Ring
     (P : Screen_Point; Radius, Inner_Radius : Integer; Color : Sc_Color) is
   begin
      for Y in -Radius .. Radius loop
         for X in -Radius .. Radius loop
            declare
               T : Integer := X * X + Y * Y;
            begin
               if T < Radius * Radius and then T >= Inner_Radius * Inner_Radius
               then
                  Set_Pixel (P.X + X, P.Y + Y, Color);
               end if;
            end;
         end loop;
      end loop;
   end Draw_Ring;


   procedure Draw_Text (Canvas : Canvas_ID; Position: Point_3d; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True) is
      SC : Screen_Point := To_Screen_Point(Internal_Canvas, (Position.X, Position.Y));
   begin
      Fonts.Draw_String(SC.X, SC.Y,
                        Str => Text,
                        Font => Font8x8,
                        FG => RGBA_To_16(Color),
                        BG   => RGBA_To_16(Bg_Color),
                        Wrap => Wrap);
   end Draw_Text;

   procedure Draw_Text (Canvas : Canvas_ID; Position: Screen_Point; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True) is
   begin
        Fonts.Draw_String(Position.X, Position.Y,
                        Str => Text,
                        Font => Font8x8,
                        FG => RGBA_To_16(Color),
                        BG   => RGBA_To_16(Bg_Color),
                        Wrap => Wrap);
   end Draw_Text;


   function Get_Text_Size(Text : String) return Screen_Point is
      P : Point := String_Size (Font8x8, Text);
   begin
      return Screen_Point'(X => P.X,
                           Y => P.Y);
   end Get_Text_Size;

--     procedure Draw_Text (P : Screen_Point; Str : String; Color : Sc_Color)
--     is
--     begin
--        Fonts.Draw_String
--          ((P.X, P.Y),
--           Str,
--           Font8x8,
--           Color,
--           Screen_Interface.Black);
--     end Draw_Text;

--     procedure Draw_Dyn_Text (P : Screen_Point; Str : String; Color : Sc_Color)
--     is
--     begin
--        Fonts.Draw_String
--          ((P.X, P.Y),
--           Str,
--           Font12x12,
--           Color,
--           Screen_Interface.Black);
--     end Draw_Dyn_Text;
   ----------
   -- Draw --
   ----------

--     procedure Draw (Id : Shape_Id) is
--        Inst : Shape := Shapes (Id);
--        SP : Screen_Point := To_Screen_Point(Internal_Canvas, (Inst.X, Inst.Y));
--        T : Natural;
--     begin
--        case Inst.Kind is
--           when Circle =>
--              T := Scale (Internal_Canvas, Inst.Radius);
--              Draw_Filled_Circle
--                (SP, T,
--                 Color_Map (Inst.Color));
--           when Torus =>
--              Draw_Ring
--                (SP, Scale (Internal_Canvas, Inst.Outer),
--                 Scale (Internal_Canvas, Inst.Inner),
--                 Color_Map (Inst.Color));
--           when Box =>
--              Draw_Box
--                (SP, Scale (Internal_Canvas, Inst.Width), Scale (Internal_Canvas, Inst.Height),
--                 Color_Map (Inst.Color));
--           when Line =>
--              Draw_Line (SP, To_Screen_Point(Internal_Canvas, (Inst.End_X, Inst.End_Y)),
--                         Color_Map (Inst.Color));
--           when Text =>
--              Draw_Text (SP,
--                         Get_Str(Inst.Str_Idx),
--                         Color_Map (Inst.Color));
--              --           when Dyn_Text =>
--              --              Draw_Dyn_Text (SP,
--              --                             Inst.Str_Ptr.all,
--              --                             Color_Map (Inst.Color));
--           when others => null;
--        end case;
--     end Draw;


   function Is_Killed return Boolean is
   begin
      return False;
   end Is_Killed;


   function Get_Cursor_Status return Cursor_T is
      S : Screen_Interface.Touch_State;
   begin
      S := Get_Touch_State;
      return (Position => (S.X, S.Y),
              Pressed => S.Touch_Detected);
   end Get_Cursor_Status;

   -----------
   -- Check --
   -----------

   procedure Check (Ret : Int) is
   begin
      if Ret /= 0 then
         raise Display_Error;
      end if;
   end Check;

   ----------
   -- Draw --
   ----------

   procedure Swap_Buffers(Window : Window_ID; Erase : Boolean := True) is
   begin
      --      Fill_Screen (SC.Black);

      --        for Id in Shapes'First .. Max_Shape_Id loop
      --           Draw (Id);
      --        end loop;

      LCD.Flip_Buffers;
      if Erase then
         Fill_Screen(Sc.Black);
      end if;
   end Swap_Buffers;

   procedure Swap_Copy_Buffers(Window : Window_ID) is
   begin
      LCD.Flip_Copy_Buffers;
   end Swap_Copy_Buffers;


   procedure Fill(Canvas : Canvas_ID; Color: RGBA_T) is
   begin
      Fill_Screen (RGBA_To_16(Color));
   end Fill;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      SC.Initialize;
   end Init;

   function Get_Zoom_Factor (Canvas : Canvas_ID) return Float is
   begin
      return Internal_Canvas.Zoom_Factor;
   end Get_Zoom_Factor;

   procedure Set_Zoom_Factor (Canvas : Canvas_ID; ZF : Float) is
   begin
      Internal_Canvas.Zoom_Factor := ZF;
   end Set_Zoom_Factor;


   Window_Created : Boolean := False;
   -- Only for STM32F429 with 320x240 screen
   function Create_Window (Width : Integer; Height : Integer; Name : String) return Window_ID is
   begin
      -- only one window 320x240 allowed
      if Width /= 240 or else Height /= 320 then
         raise Display_Error;
      end if;
      if Window_Created then
         raise Too_Many_Windows;
      end if;

      Window_Created := True;
      return Window_ID'First;
   end Create_Window;

   function Get_Canvas(Window : Window_ID) return Canvas_ID is
   begin

      if Window /= Window_ID'First then
         raise Display_Error;
      end if;

      return Canvas_ID'First;
   end Get_Canvas;

   -------------------------
   -- NOT YET IMPLEMENTED --
   -------------------------

   --     function New_Text
   --       (X     : Float;
   --        Y     : Float;
   --        Text  : String;
   --        Color : Color_Type)
   --        return Shape_Id is (Null_Shape_Id);
   --     procedure Set_Text (V : in out Shape_Id; Text : String) is null;

   --     function Get_Text (V : Shape_Id) return String is ("");

   function Read_Last_Mouse_Position return Mouse_Position
   is (No_Mouse_Position);
   function At_End return Boolean is (False);


   procedure Enable_3d_Light (Canvas : Canvas_ID) is
   begin
      -- does not do anything without opengl
      null;
   end Enable_3d_Light;

   procedure Disable_3d_Light (Canvas : Canvas_ID) is
   begin
      -- does not do anything without opengl
      null;
   end Disable_3d_Light;

   procedure Set_3d_Light (Canvas : Canvas_ID;
                           Position : Point_3d;
                           Diffuse_Color : RGBA_T;
                           Ambient_Color : RGBA_T) is
   begin
      -- does not do anything without opengl
      null;
   end Set_3d_Light;



begin
   Init;
end Display.Basic;
