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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with SDL_stdinc_h; use SDL_stdinc_h;
with SDL_video_h; use SDL_video_h;
with System;
with Ada.Text_IO; use Ada.Text_IO;
with SDL_error_h; use SDL_error_h;
with SDL_h; use SDL_h;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;
with Interfaces; use Interfaces;
with SDL_events_h; use SDL_events_h;
with Ada.Real_Time; use Ada.Real_Time;
with Display.Basic.Fonts; use Display.Basic.Fonts;
with Display.Basic.Utils; use Display.Basic.Utils;
with SDL_surface_h; use SDL_surface_h;
with SDL_pixels_h; use SDL_pixels_h;

package body Display.Basic is

   ---------------
   -- SDL STATE --
   ---------------

   --Surface     : access SDL_Surface;
   --     Vid_Info    : access SDL_VideoInfo;
   --     Sdl_Width   : constant Integer := 800;
   --     Sdl_Height  : constant Integer := 800;
   --     BPP         : constant Interfaces.C.int := 32;
   --     Sdl_Flags   : constant Interfaces.C.unsigned :=
   --       SDL_HWSURFACE + SDL_RESIZABLE + SDL_DOUBLEBUF;

   Initialized : boolean := False with Atomic, Volatile;

   type Cart_Point is record
      X, Y : Float;
   end record;


   --     function "+" (P : Cart_Point; S : SDL_Surface) return Screen_Point
   --     is   ((X => Integer (C.Zoom_Factor * P.X) + Integer(C.Surface.w / 2) + C.Center.X,
   --Y => Integer(C.Surface.h / 2) - C.Center.Y - Integer (C.Zoom_Factor * P.Y)));

   --       ((X => Integer (Zoom_Factor * P.X) + Integer(S.w / 2),
   --         Y => Integer(S.h / 2) - Integer (Zoom_Factor * P.Y)));



   function To_Screen_Point (C : T_Internal_Canvas; P : Cart_Point) return Screen_Point
   is
     ((X => Integer (C.Zoom_Factor * P.X) + Integer(C.Surface.w / 2) - C.Center.X,
       Y => Integer(C.Surface.h / 2) + C.Center.Y - Integer (C.Zoom_Factor * P.Y)));


   function To_Point3d (Canvas : Canvas_ID; P : Screen_Point) return Point_3d is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      return (Float(P.X - Integer(C.Surface.w / 2) - C.Center.X),
              Float(Integer(C.Surface.h / 2) - C.Center.Y - P.Y),
              0.0);
   end To_Point3d;

   function Get_Zoom_Factor(Canvas : Canvas_ID)  return Float is (Get_Internal_Canvas(Canvas).Zoom_Factor);
   procedure Set_Zoom_Factor (Canvas : Canvas_ID; ZF : Float) is

   begin
      Display.Basic.Utils.Set_Zoom_Factor(Canvas, ZF);
   end Set_Zoom_Factor;


   function To_Screen_Point (Canvas : Canvas_ID;  P : Point_3d) return Screen_Point is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      return To_Screen_Point(C, (P.X, P.Y));
   end To_Screen_Point;

   function Scale (Canvas: T_Internal_Canvas; L : Float) return Integer is (Integer (L * Canvas.Zoom_Factor));


   procedure Set_Center (Canvas : Canvas_ID; Position : Point_3d) is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Display.Basic.Utils.Set_Center(Canvas, (Scale(C, Position.X), Scale(C, Position.Y)));
   end Set_Center;

   procedure Set_Center (Canvas : Canvas_ID;  Position : Screen_Point) is
   begin
      Display.Basic.Utils.Set_Center(Canvas, Position);
   end Set_Center;


   function Get_Center (Canvas : Canvas_ID) return Screen_Point is
   begin
      return Display.Basic.Utils.Get_Center(Canvas);
   end Get_Center;

   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle (Surface : access SDL_Surface; P : Screen_Point; Radius : Integer; Color : RGBA_T)
   is
      CX, CY, Radius_Error : Integer;
      UC : Uint32 := RGBA_To_Uint32(Surface, Color);
   begin

      if SDL_LockSurface (Surface) < 0 then
         raise Display_Error;
      end if;

      CX := Radius;
      CY := 0;
      Radius_Error := 1 - CX;

      while CX >= CY loop
         Put_Pixel (Surface,  CX + P.X,  CY + P.Y, UC);
         Put_Pixel (Surface,  CY + P.X,  CX + P.Y, UC);
         Put_Pixel (Surface, -CX + P.X,  CY + P.Y, UC);
         Put_Pixel (Surface, -CY + P.X,  CX + P.Y, UC);
         Put_Pixel (Surface, -CX + P.X, -CY + P.Y, UC);
         Put_Pixel (Surface, -CY + P.X, -CX + P.Y, UC);
         Put_Pixel (Surface,  CX + P.X, -CY + P.Y, UC);
         Put_Pixel (Surface,  CY + P.X, -CX + P.Y, UC);
         CY := CY + 1;
         if Radius_Error < 0 then
            Radius_Error := Radius_Error + (2 * CY + 1);
         else
            CX := CX - 1;
            Radius_Error := Radius_Error + (2 * (CY - CX) + 1);
         end if;
      end loop;
      SDL_UnlockSurface (Surface);

   end Draw_Circle;


   procedure Draw_Circle (Canvas : Canvas_ID; Position: Point_3d; Radius : Float; Color: RGBA_T) is
      C : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
   begin
      Draw_Circle(C.Surface,
                  To_Screen_Point(C, (Position.X, Position.Y)),
                  Scale (C, Radius),
                  Color);
   end Draw_Circle;

   procedure Draw_Circle (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T) is
      C : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
   begin
      Draw_Circle(C.Surface,
                  Position,
                  Radius,
                  Color);
   end Draw_Circle; ---------------
   -- Draw_Line --
   ---------------

   --     procedure Draw_Line
   --       (Surface : access SDL_Surface; P_Start, P_End : Screen_Point; Color : RGBA_Color)
   --     is
   --        DX : Integer := P_End.X - P_Start.X;
   --        DY : Integer := P_End.Y - P_Start.Y;
   --        D  : Integer := 2 * DY - DX;
   --        Y  : Integer := P_Start.Y;
   --     begin
   --        Put_Pixel (Surface, P_Start.X, P_Start.Y, Color);
   --        for X in P_Start.X +1 .. P_End.X loop
   --           if D > 0 then
   --              Y := Y + 1;
   --              Put_Pixel (Surface, X, Y, Color);
   --              D := D + (2 * DY - 2 * DX);
   --           else
   --              Put_Pixel (Surface, X, Y, Color);
   --              D := D + (2 * DY);
   --           end if;
   --        end loop;
   --     end Draw_Line;

   procedure Draw_Line (Surface : access SDL_Surface; P0 : Screen_Point; P1 : Screen_Point; Color : Uint32) is
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
         Put_Pixel (Surface, X, Y, Color);
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



   procedure Draw_Line (Surface : access SDL_Surface; P0 : Screen_Point; P1 : Screen_Point; Color : RGBA_T) is
   begin
      Draw_Line(Surface, P0, P1, RGBA_To_Uint32(Surface, Color));
   end Draw_Line;

   procedure Draw_Line (Canvas : Canvas_ID; P1: Point_3d; P2 : Point_3d; Color: RGBA_T) is
      C : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
   begin
      Draw_Line (C.Surface,
                 To_Screen_Point(C, (P1.X, P1.Y)),
                 To_Screen_Point(C, (P2.X, P2.Y)),
                 Color);

   end Draw_Line;

   procedure Draw_Line (Canvas : Canvas_ID; P1: Screen_Point; P2 : Screen_Point; Color: RGBA_T) is
      C : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
   begin
      Draw_Line (C.Surface,
                 P1,
                 P2,
                 Color);

   end Draw_Line;



   procedure Draw_Rect
     (Surface: access SDL_Surface; P : Screen_Point; Width, Height : Integer; Color : RGBA_T)
   is
      UC : Uint32 := RGBA_To_Uint32(Surface, Color);
   begin
      Draw_Line(Surface, P, (P.x + Width - 1, P.Y), UC);
      Draw_Line(Surface, P, (P.x, P.Y + Height - 1), UC);
      Draw_Line(Surface, (P.x + Width - 1, P.Y), (P.x + Width - 1, P.Y + Height - 1), UC);
      Draw_Line(Surface, (P.x, P.Y + Height - 1), (P.x + Width - 1, P.Y + Height - 1), UC);
   end Draw_Rect;

   procedure Draw_Fill_Rect
     (Surface: access SDL_Surface; P : Screen_Point; Width, Height : Integer; Color : RGBA_T)
   is
      UC : Uint32 := RGBA_To_Uint32(Surface, Color);
   begin
      for I in 0 .. Width - 1 loop
         for J in 0 .. Height - 1 loop
            Put_Pixel (Surface, P.X + I, P.Y + J, UC);
         end loop;
      end loop;
   end Draw_Fill_Rect;

   procedure Draw_Rect(Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T) is
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Draw_Rect(IC.Surface,
                To_Screen_Point(IC, (Position.X, Position.Y)),
                Scale(IC, Width),
                Scale (IC, Height),
                Color);
   end Draw_Rect;

   procedure Draw_Rect(Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T) is
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Draw_Rect(IC.Surface,
                Position,
                Width,
                Height,
                Color);
   end Draw_Rect;


   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T) is
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Draw_Fill_Rect(IC.Surface,
                     To_Screen_Point(IC, (Position.X, Position.Y)),
                     Scale(IC, Width),
                     Scale (IC, Height),
                     Color);
   end Draw_Fill_Rect;

   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T) is
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Draw_Fill_Rect(IC.Surface,
                     Position,
                     Width,
                     Height,
                     Color);
   end Draw_Fill_Rect;

   ---------------


   ------------------------
   -- Draw_Filled_Circle --
   ------------------------

   --     procedure Draw_Filled_Circle
   --       (Surface : access SDL_Surface; P : Screen_Point; Radius : Integer; Color : RGBA_Color)
   --     is
   --     begin
   --        if SDL_LockSurface (Surface) < 0 then
   --           raise Display_Error;
   --        end if;
   --
   --        for Y in -Radius .. Radius loop
   --           for X in -Radius .. Radius loop
   --              if X * X + Y * Y < Radius * Radius then
   --                 Put_Pixel (Surface, P.X + X, P.Y + Y, Color);
   --              end if;
   --           end loop;
   --        end loop;
   --        SDL_UnlockSurface (Surface);
   --
   --     end Draw_Filled_Circle;


   procedure Draw_Filled_Circle (Surface : access SDL_Surface; P : Screen_Point; Radius : Integer; Color : RGBA_T) is
      r                  : Integer := Radius;
      x                  : Integer := -r;
      y                  : Integer := 0;
      err                : Integer := 2 - 2 * r; --/  * II. Quadrant *  /
      UC : Uint32 := RGBA_To_Uint32(Surface, Color);
   begin
      if SDL_LockSurface (Surface) < 0 then
         raise Display_Error;
      end if;
      if Radius <= 1 then
         Put_Pixel (Surface, P.X, P.Y, UC);
      else

         while x < 0 loop

            Draw_Line (Surface, (P.X - x, P.Y - y), (P.X + x, P.Y - y), UC);
            Draw_Line (Surface, (P.X - x, P.Y + y), (P.X + x, P.Y + y), UC);

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
      SDL_UnlockSurface (Surface);

   end Draw_Filled_Circle;

   procedure Draw_Sphere (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T)is
      C : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
   begin
      Draw_Filled_Circle(C.Surface,
                         Position,
                         Radius,
                         Color);
   end Draw_Sphere;


   procedure Draw_Sphere (Canvas : Canvas_ID; Position: Point_3d; Radius : Float; Color: RGBA_T) is
      C : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
   begin
      Draw_Filled_Circle(C.Surface,
                         To_Screen_Point(C, (Position.X, Position.Y)),
                         Scale (C, Radius),
                         Color);
   end Draw_Sphere;

   ---------------
   -- Draw_Ring --
   ---------------

   procedure Draw_Ring
     (Surface : access SDL_Surface; P : Screen_Point; Radius, Inner_Radius : Integer; Color : RGBA_T) is
      UC : Uint32 := RGBA_To_Uint32(Surface, Color);
   begin
      for Y in -Radius .. Radius loop
         for X in -Radius .. Radius loop
            declare
               T : Integer := X * X + Y * Y;
            begin
               if T < Radius * Radius and then T >= Inner_Radius * Inner_Radius
               then
                  Put_Pixel (Surface, P.X + X, P.Y + Y, UC);
               end if;
            end;
         end loop;
      end loop;
   end Draw_Ring;



   --     ----------
   --     -- Draw --
   --     ----------
   --
   --     procedure Draw (Canvas: T_Internal_Canvas; Inst : Shape) is
   --     begin
   --        case Inst.Kind is
   --           when Circle =>
   --              Draw_Filled_Circle
   --                (Canvas.Surface,
   --                 To_Screen_Point(Canvas, (Inst.X, Inst.Y)),
   --                 Scale (Canvas, Inst.Radius),
   --                 Color_Map (Inst.Color));
   --           when Torus =>
   --              Draw_Ring
   --                (Canvas.Surface,
   --                 To_Screen_Point(Canvas, (Inst.X, Inst.Y)), Scale (Canvas, Inst.Outer),
   --                 Scale (Canvas, Inst.Inner),
   --                 Color_Map (Inst.Color));
   --           when Box =>
   --              Draw_Box
   --                (Canvas.Surface,
   --                 To_Screen_Point (Canvas, (Inst.X, Inst.Y)),
   --                 Scale (Canvas, Inst.Width),
   --                 Scale (Canvas, Inst.Height),
   --                 Color_Map (Inst.Color));
   --           when Line =>
   --              Draw_Line (Canvas.Surface,
   --                         To_Screen_Point(Canvas, (Inst.X, Inst.Y)),
   --                         To_Screen_Point(Canvas, (Inst.End_X, Inst.End_Y)),
   --                         Color_Map (Inst.Color));
   --           when others => null;
   --        end case;
   --     end Draw;

   -----------
   -- Check --
   -----------

   procedure Check (Ret : Int) is
   begin
      if Ret /= 0 then
         raise Display_Error;
      end if;
   end Check;

   procedure Poll_Events;


   Internal_Cursor : Cursor_T := ((0,0), False);
   Killed : Boolean := False;

   function Is_Killed return Boolean is
   begin
      return Killed;
   end Is_Killed;

   procedure Poll_Events is
      E : aliased SDL_Event;
   begin
      while SDL_PollEvent (E'Access) /= 0 loop
         case unsigned (E.c_type) is
            when SDL_events_h.SDL_QUIT_Evt =>
               Killed := True;
               SDL_h.SDL_Quit;
            when SDL_events_h.SDL_MOUSEBUTTONDOWN =>
               Internal_Cursor.Position := (Integer(E.motion.x), Integer(E.motion.y));
               Internal_Cursor.Pressed := True;
            when SDL_events_h.SDL_MOUSEBUTTONUP =>
               Internal_Cursor.Position := (Integer(E.motion.x), Integer(E.motion.y));
               Internal_Cursor.Pressed := False;

            when others =>
               null;
         end case;
      end loop;
   end Poll_Events;



   function Get_Cursor_Status return Cursor_T is
   begin
      Poll_Events;
      return Internal_Cursor;
   end Get_Cursor_Status;



   procedure Draw_Text (Canvas : Canvas_ID; Position: Point_3d; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True) is
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Draw_String(IC,
                  P    => To_Screen_Point(IC, (Position.X, Position.Y)),
                  Str  => Text,
                  Font => Font8x8,
                  FG   => Color,
                  BG   => Bg_Color,
                  Wrap => Wrap);
   end Draw_Text;

   procedure Draw_Text (Canvas : Canvas_ID; Position: Screen_Point; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True) is
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Draw_String(IC,
                  P    => Position,
                  Str  => Text,
                  Font => Font8x8,
                  FG   => Color,
                  BG   => Bg_Color,
                  Wrap => Wrap);
   end Draw_Text;

   function Get_Text_Size(Text : String) return Screen_Point is
   begin
      return String_Size (Font8x8, Text);
   end Get_Text_Size;


   procedure Set_Pixel (Canvas : Canvas_ID; Position : Screen_Point; Color : RGBA_T) is
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Display.Basic.Utils.Put_Pixel_Slow(Screen => IC.Surface,
                                         X      => Position.X,
                                         Y      => Position.Y,
                                         Color  => Color);
   end Set_Pixel;



   -------------------
   -- Set_SDL_Video --
   -------------------
   type SDL_Window_Surface is record
      surface : Canvas_ID;
      window : access SDL_Window;
      w       : Integer := 400;
      h       : Integer := 400;
      bpp   :  Interfaces.C.int := 32;
      --  flags :  Interfaces.C.unsigned :=   SDL_HWSURFACE + SDL_RESIZABLE + SDL_DOUBLEBUF;
   end record;


   type Windows_Array is array (Window_ID) of SDL_Window_Surface;
   Nb_Windows : Integer := 0;

   Stored_Windows : Windows_Array;

   function Create_SDL_Window (Width : Integer; Height : Integer; Name : String) return SDL_Window_Surface;

   function Create_Window (Width : Integer; Height : Integer; Name : String) return Window_ID is
      Current_Id : Window_ID;
   begin
      if Nb_Windows = Windows_Array'Length then
         raise Too_Many_Windows;
      end if;
      Current_Id := Window_ID (Integer (Window_ID'First) + Nb_Windows);


      Stored_Windows(Current_Id) := Create_SDL_Window (Width, Height, Name);
      Nb_Windows := Nb_Windows + 1;
      return Current_Id;
   end Create_Window;


   function Create_SDL_Window (Width : Integer; Height : Integer; Name : String) return SDL_Window_Surface is
      S : SDL_Window_Surface;
      SDL_S : access SDL_Surface;
      PF : Uint32;
      CP : Interfaces.C.Strings.chars_ptr;
   begin
      if not Initialized then
         raise Graphical_Context_Not_Initialized;
      end if;

      --  To center a non-fullscreen window we need to set an environment
      --  variable
      S.window := SDL_CreateWindow(New_String(Name), SDL_WINDOWPOS_CENTERED,
                                   SDL_WINDOWPOS_CENTERED,
                                   Interfaces.C.int(Width),
                                   Interfaces.C.int(Height),
                                   SDL_WINDOW_SHOWN);
      PF := SDL_GetWindowPixelFormat(S.window);
      CP := SDL_GetPixelFormatName(PF);
      --      Ada.Text_IO.Put_Line ("screen pixel format = " & Integer'Image(Integer(PF)) & " : " &  Value (CP));

      --  the setVideoMode function returns the current frame buffer as an
      --  SDL_Surface. Again, we grab a pointer to it, then place its
      --  content into the non pointery surface variable. I say 'non-pointery',
      --  but this SDL variable must have a pointer in it because it can
      --  access the current pixels in the framebuffer.

      S.w := Width;
      S.h := Height;

      SDL_S := SDL_GetWindowSurface(S.window);

      if SDL_S = null then
         Put_Line ("Error setting the video mode");
         Put_Line(Value (SDL_GetError));
         SDL_h.SDL_Quit;
         raise Graphical_Context_Not_Initialized;
      end if;

      S.surface := Register_SDL_Surface(SDL_S);

      return S;

   end Create_SDL_Window;


   function Get_Canvas(Window : Window_ID) return Canvas_ID is
   begin
      return Stored_Windows(Window).surface;
   end Get_Canvas;

   function Get_Canvas_Size(Canvas : Canvas_ID) return Screen_Point is
      IC : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
   begin
      return Screen_Point'(Integer(IC.Surface.w), Integer(IC.Surface.h));
   end Get_Canvas_Size;

   ----------
   -- Draw --
   ----------

   procedure Swap_Buffers (Window : Window_ID; Erase : Boolean := True) is
      Canvas : T_Internal_Canvas := Get_Internal_Canvas(Get_Canvas(Window));
   begin

      --        if SDL_LockSurface (Canvas.Surface) < 0 then
      --           raise Display_Error;
      --        end if;
      --
      --
      --        for Id in Shapes'First .. Max_Shape_Id loop
      --           Draw (Canvas, Shapes (Id));
      --        end loop;
      --        SDL_UnlockSurface (Canvas.Surface);

      if SDL_UpdateWindowSurface (Stored_Windows(Window).window) < 0 then
         raise Display_Error;
      end if;
      if Erase then
         if SDL_FillRect (Canvas.Surface, null, 0) < 0 then
            raise Display_Error;
         end if;
      end if;
      Poll_Events;
   end Swap_Buffers;

   procedure Swap_Copy_Buffers (Window : Window_ID) is
   begin
      Swap_Buffers(Window, False);
   end Swap_Copy_Buffers;

   procedure Fill(Canvas : Canvas_ID; Color: RGBA_T) is
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
      UC : Uint32 := RGBA_To_Uint32(IC.Surface, Color);
   begin
      if SDL_FillRect (IC.Surface, null, UC) < 0 then
         raise Display_Error;
      end if;
   end Fill;




   procedure Init is
   begin
      --  SDL is comprised of 8 subsystems. Here we initialize the video
      if SDL_Init(SDL_INIT_VIDEO) < 0 then
         Put_Line ("Error initializing SDL");
         Put_Line(Value (SDL_GetError));
         SDL_h.SDL_Quit;
         raise Graphical_Context_Not_Initialized;
      end if;

      Initialized := True;

   end Init;

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
   --     function Current_Key_Press return Key_Type is (0);
   --     function To_Character (Key : Key_Type) return Character is (' ');
   --     function To_Special (Key : Key_Type) return Special_Key is (KEY_NONE);
   --     function Is_Special_Key (Key : Key_Type) return Boolean is (False);
   --     function Is_Control_Key (Key : Key_Type) return Boolean is (False);
   --     function Is_Shift_Key (Key : Key_Type) return Boolean is (False);
   --     function Is_Alt_Key (Key : Key_Type) return Boolean is (False);
   --     function Read_Last_Mouse_Position return Mouse_Position
   --     is (No_Mouse_Position);
   --     function At_End return Boolean is (False);

begin
   Init;
end Display.Basic;
