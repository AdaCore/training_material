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

with Display.Kernel;  use Display.Kernel;
with GL_gl_h; use GL_gl_h;
with Display.Basic.Fonts; use Display.Basic.Fonts;
with Display.Basic.Utils; use Display.Basic.Utils;
with Interfaces.C; use Interfaces.C;
with SDL_SDL_events_h; use SDL_SDL_events_h;
with SDL_SDL_video_h; use SDL_SDL_video_h;
with Display.Basic.GLFonts; use Display.Basic.GLFonts;

package body Display.Basic is



   function Is_Killed return Boolean is
   begin
      return Is_Stopped;
   end Is_Killed;

--     procedure Poll_Events is
--        E : aliased SDL_Event;
--     begin
--        while SDL_PollEvent (E'Access) /= 0 loop
--           case unsigned (E.c_type) is
--              when SDL_SDL_events_h.SDL_Quit =>
--                 Killed := True;
--                 SDL_SDL_h.SDL_Quit;
--              when SDL_SDL_events_h.SDL_MOUSEBUTTONDOWN =>
--                 Internal_Cursor.Position := (Integer(E.motion.x), Integer(E.motion.y));
--                 Internal_Cursor.Pressed := True;
--              when SDL_SDL_events_h.SDL_MOUSEBUTTONUP =>
--                 Internal_Cursor.Position := (Integer(E.motion.x), Integer(E.motion.y));
--                 Internal_Cursor.Pressed := False;
--
--              when others =>
--                 null;
--           end case;
--        end loop;
--     end Poll_Events;
--



--     function At_End return Boolean is
--     begin
--        return Display.Kernel.At_End;
--     end At_End;

   ----------
   -- Draw --
   ----------

   type Windows_Array is array (Window_ID) of OpenGL_Surface;
   Nb_Windows : Integer := 0;

   Stored_Windows : Windows_Array;


   function Create_Window (Width : Integer; Height : Integer; Name : String) return Window_ID is
      Current_Id : Window_ID;
   begin
      if Nb_Windows = Windows_Array'Length then
         raise Too_Many_Windows;
      end if;
      Current_Id := Window_ID (Integer (Window_ID'First) + Nb_Windows);
      Stored_Windows(Current_Id) := Display.Kernel.Create_Window (Width, Height, Name);

      Nb_Windows := Nb_Windows + 1;
      return Current_Id;
   end Create_Window;


   procedure Swap_Buffers(Window : Window_ID; Erase : Boolean := True) is
   begin
      Display.Kernel.Swap_Buffers(Stored_Windows(Window));
   end Swap_Buffers;

   procedure Swap_Copy_Buffers(Window : Window_ID) is
   begin
      -- to be implemented properly
      Display.Kernel.Swap_Buffers(Stored_Windows(Window));
   end Swap_Copy_Buffers;


   function Get_Zoom_Factor(Canvas : Canvas_ID) return Float is
   begin
       return  Get_Internal_Canvas(Canvas).Zoom_Factor;
   end Get_Zoom_Factor;



   procedure Set_Zoom_Factor (Canvas : Canvas_ID; ZF : Float) is
     C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Display.Basic.Utils.Set_Zoom_Factor(Canvas, ZF);
      UpdateProjection(Canvas);
   end Set_Zoom_Factor;


   function Get_Canvas(Window : Window_ID) return Canvas_ID is
   begin
      return Stored_Windows(Window).Canvas;
   end Get_Canvas;

   procedure Set_Color(Color : RGBA_T) is
   begin
      glColor3d (double(Color.R) / 255.0,
                 double(Color.G) / 255.0,
                 double(Color.B) / 255.0);
   end Set_Color;

   procedure Fill(Canvas : Canvas_ID; Color: RGBA_T)
   is
   begin
      raise Display_Error;
   end Fill;


   procedure Draw_Sphere (Canvas : Canvas_ID; Position : Point_3d; Radius : Float; Color: RGBA_T) is
      IC : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
   begin
      glPushMatrix;

      glTranslated
        (GLdouble (Position.X - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Y - (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Z));


      Set_Color (Color);

       if Radius < 0.1 then
         Disable_3d_Light(Canvas_ID'First);
         glBegin(GL_POINTS);

         glVertex2i(0, 0);
         glEnd;
         Enable_3d_Light(Canvas_ID'First);
      else
         Display.Kernel.Draw_Sphere(Radius, Color);


      end if;

      glPopMatrix;
   end Draw_Sphere;

   procedure Draw_Sphere (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T) is
   begin
      raise Display_Error;
   end Draw_Sphere;




   procedure Draw_Circle (Canvas : Canvas_ID; Position: Point_3d; Radius : Float; Color: RGBA_T) is
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin

      glPushMatrix;

      glTranslated
        (GLdouble (Position.X - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Y - (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Z));

      Set_Color (Color);
      Disable_3d_Light(Canvas);

      glShadeModel(GL_SMOOTH);
      glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
      glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
      glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
      glEnable(GL_LINE_SMOOTH);
      glEnable(GL_POLYGON_SMOOTH);
      glEnable(GL_POINT_SMOOTH);

      DrawDisk(InnerRadius => Radius,
               OuterRadius => Radius + 1.0,
               VSlices     => 40,
               HSlices     => 40,
               Color       => Color);
      Enable_3d_Light (Canvas);
      glPopMatrix;
   end Draw_Circle;

   procedure Draw_Circle (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T) is
   begin
      raise Display_Error;
   end Draw_Circle;

   procedure Draw_Line (Canvas : Canvas_ID; P1: Point_3d; P2 : Point_3d; Color: RGBA_T) is
   begin
      raise Display_Error;
   end Draw_Line;

   procedure Draw_Line (Canvas : Canvas_ID; P1: Screen_Point; P2 : Screen_Point; Color: RGBA_T) is
   begin
      raise Display_Error;
   end Draw_Line;

   procedure Draw_Text (Canvas : Canvas_ID; Position: Point_3d; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True) is
      --  S : access SDL_Surface;
      --Format : UInt32;
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
      Cursor : Screen_Point := (0, 0);
      Font   : BMP_Font := Font8x8;
      w : Integer := Char_Size (Font).X;
      h : Integer := Char_Size (Font).Y;


   begin
      glPushMatrix;

--        glTranslated
--          (GLdouble (Position.X),
--           GLdouble (Position.Y),
--           GLdouble (Position.Z));
      glTranslated
        (GLdouble (Position.X - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Y - (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Z));


      glScalef(1.0/IC.Zoom_Factor,
               1.0/IC.Zoom_Factor,
               1.0/IC.Zoom_Factor);

      --glMatrixMode(GL_PROJECTION);
      --glPushMatrix;
      --glLoadIdentity;
     -- glOrtho(0.0, double(IC.Surface.w),
       --         double(IC.Surface.h), 0.0, 0.0, 1.0);

      Disable_3d_Light(Canvas);
      glDisable(GL_DEPTH_TEST);

      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
--      glBlendFunc(GL_SRC_ALPHA, GL_ONE);--_MINUS_SRC_ALPHA);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

      Set_Color (Color);

      for C of Text loop
         glBindTexture(GL_TEXTURE_2D, getCharTexture(C));

         glBegin(GL_QUADS);
         glTexCoord2f(0.0, 1.0); glVertex3f(Float(Cursor.X), Float(Cursor.Y), 0.0);
         glTexCoord2f(0.0, 0.0); glVertex3f(Float(Cursor.X), Float(Cursor.Y) + Float(h), 0.0);
         glTexCoord2f(1.0, 0.0); glVertex3f(Float(Cursor.X) + Float(w), Float(Cursor.Y) + Float(h), 0.0);
         glTexCoord2f(1.0, 1.0); glVertex3f(Float(Cursor.X) + Float(w), Float(Cursor.Y), 0.0);
         glEnd;

         if Cursor.X + Char_Size (Font).X > Integer(IC.Surface.w) then
            if Wrap then
               Cursor.Y := Cursor.Y + h;
               Cursor.X := 0;
            else
               exit;
            end if;
         else
            Cursor.X := Cursor.X + w;
         end if;

      end loop;

      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
      glEnable(GL_DEPTH_TEST);
      Enable_3d_Light(Canvas);


--        glPopMatrix;
--        glMatrixMode(GL_MODELVIEW);
--        glLoadIdentity;

      glPopMatrix;
   end Draw_Text;

   procedure Draw_Text (Canvas : Canvas_ID; Position: Screen_Point; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True) is
          --Format : UInt32 := SDL_PIXELFORMAT_RGBA32;
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
      Cursor : Screen_Point := Position;
      Font   : BMP_Font := Font8x8;
      w : Integer := Char_Size (Font).X;
      h : Integer := Char_Size (Font).Y;

   begin
      Disable_3d_Light(Canvas);
      glDisable(GL_DEPTH_TEST);

      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
--      glBlendFunc(GL_SRC_ALPHA, GL_ONE);--_MINUS_SRC_ALPHA);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glOrtho(0.0, double(IC.Surface.w),
                double(IC.Surface.h), 0.0, 0.0, 1.0);

      Set_Color (Color);

      for C of Text loop
         glBindTexture(GL_TEXTURE_2D, getCharTexture(C));

         glBegin(GL_QUADS);
         glTexCoord2f(0.0, 0.0); glVertex3f(Float(Cursor.X), Float(Cursor.Y), 0.0);
         glTexCoord2f(0.0, 1.0); glVertex3f(Float(Cursor.X), Float(Cursor.Y) + Float(h), 0.0);
         glTexCoord2f(1.0, 1.0); glVertex3f(Float(Cursor.X) + Float(w), Float(Cursor.Y) + Float(h), 0.0);
         glTexCoord2f(1.0, 0.0); glVertex3f(Float(Cursor.X) + Float(w), Float(Cursor.Y), 0.0);
         glEnd;

         if Cursor.X + Char_Size (Font).X > Integer(IC.Surface.w) then
            if Wrap then
               Cursor.Y := Cursor.Y + h;
               Cursor.X := 0;
            else
               exit;
            end if;
         else
            Cursor.X := Cursor.X + w;
         end if;

      end loop;

      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;

      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
      glEnable(GL_DEPTH_TEST);
      Enable_3d_Light(Canvas);
   end Draw_Text;

   procedure Draw_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T) is
   begin
      null;
--      raise Display_Error;
   end Draw_Rect;

   procedure Draw_Rect (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T) is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
       glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glOrtho(0.0, double(C.Surface.w),
                double(C.Surface.h), 0.0, 0.0, 1.0);
      Disable_3d_Light (Canvas);
      Set_Color (Color);

      glRecti(int (Position.X),
              int (Position.Y),
              int (Position.X + Width),
              int (Position.Y + Height));

      Enable_3d_Light (Canvas);
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
   end Draw_Rect;

   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T) is
   begin
      raise Display_Error;
   end Draw_Fill_Rect;

   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T) is
    C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glOrtho(0.0, double(C.Surface.w),
                double(C.Surface.h), 0.0, 0.0, 1.0);
      Disable_3d_Light (Canvas);
      Set_Color (Color);

      glRecti(int (Position.X),
              int (Position.Y),
              int (Position.X + Width),
              int (Position.Y + Height));

      Enable_3d_Light (Canvas);
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
   end Draw_Fill_Rect;


   procedure Set_Pixel (Canvas : Canvas_ID; Position : Screen_Point; Color : RGBA_T) is
   begin
      raise Display_Error;
   end Set_Pixel;



   function Get_Text_Size(Text : String) return Screen_Point is
   begin
      return String_Size (Font8x8, Text);
   end Get_Text_Size;

   function Scale (Canvas: T_Internal_Canvas; L : Float) return Integer is (Integer (L * Canvas.Zoom_Factor));

   procedure Set_Center (Canvas : Canvas_ID; Position : Point_3d)  is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Display.Basic.Utils.Set_Center(Canvas, (Scale(C, Position.X), Scale(C, Position.Y)));
   end Set_Center;

   procedure Set_Center (Canvas : Canvas_ID; Position : Screen_Point)  is
   begin
      Display.Basic.Utils.Set_Center(Canvas, Position);
   end Set_Center;

   function Get_Center (Canvas : Canvas_ID) return Screen_Point  is
   begin
      return Display.Basic.Utils.Get_Center(Canvas);
   end Get_Center;




   function Get_Cursor_Status return Cursor_T is
   begin
      return Get_Internal_Cursor;
   end Get_Cursor_Status;


   function To_Point3d (Canvas : Canvas_ID; P : Screen_Point) return Point_3d is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      return (Float(P.X - Integer(C.Surface.w / 2) - C.Center.X),
              Float(Integer(C.Surface.h / 2) - C.Center.Y - P.Y),
              0.0);
   end To_Point3d;

   function To_Screen_Point (Canvas : Canvas_ID;  P : Point_3d) return Screen_Point is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      return Screen_Point'(X => Integer (C.Zoom_Factor * P.X) + Integer(C.Surface.w / 2) - C.Center.X,
                           Y => Integer(C.Surface.h / 2) + C.Center.Y - Integer (C.Zoom_Factor * P.Y));

   end To_Screen_Point;

   function Get_Canvas_Size(Canvas : Canvas_ID) return Screen_Point is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      return Screen_Point'(Integer(C.Surface.w), Integer(C.Surface.h));
   end Get_Canvas_Size;



 procedure Set_3d_Light (Canvas : Canvas_ID;
                           Position : Point_3d;
                           Diffuse_Color : RGBA_T;
                           Ambient_Color : RGBA_T) is
      type GLFloat_Array is array (Natural range <>) of aliased GLfloat;
      light_position : aliased GLFloat_Array
        := (Position.X, Position.Y, Position.Z, 1.0);
      light_diffuse : aliased GLFloat_Array
        := (Float (Diffuse_Color.R) / 255.0,
            Float (Diffuse_Color.G) / 255.0,
            Float (Diffuse_Color.B) / 255.0, 1.0);
      light_ambient : aliased GLFloat_Array :=
        (Float (Ambient_Color.R) / 255.0,
         Float (Ambient_Color.G) / 255.0,
         Float (Ambient_Color.B) / 255.0, 1.0);
            IC : T_Internal_Canvas := Get_Internal_Canvas (Canvas);

   begin
      glPushMatrix;
      glTranslated
        (GLdouble (- (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
         GLdouble (- (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
         GLdouble (0.0));
      glLightfv (GL_LIGHT0, GL_POSITION, light_position (0)'Access);
      glLightfv (GL_LIGHT0, GL_DIFFUSE, light_diffuse (0)'Access);
      glLightfv (GL_LIGHT0, GL_AMBIENT, light_ambient (0)'Access);
      glPopMatrix;
   end Set_3d_Light;

   procedure Enable_3d_Light (Canvas : Canvas_ID) is
   begin
      glEnable (GL_LIGHTING);
      glEnable (GL_LIGHT0);
   end Enable_3d_Light;

   procedure Disable_3d_Light (Canvas : Canvas_ID) is
   begin
      glDisable (GL_LIGHTING);
      glDisable (GL_LIGHT0);
   end Disable_3d_Light;


end Display.Basic;
