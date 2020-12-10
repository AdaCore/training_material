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

--with Display.Kernel;  use Display.Kernel;
with SDL_surface_h; use SDL_surface_h;
with SDL_video_h; use SDL_video_h;
with SDL_surface_h; use SDL_surface_h;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with SDL_error_h; use SDL_error_h;
with SDL_h; use SDL_h;
with SDL_events_h; use SDL_events_h;
with GL_gl_h; use GL_gl_h;
with GL_glu_h; use GL_glu_h;
with System;
use System;
with Display.Basic.Utils; use Display.Basic.Utils;
with SDL_stdinc_h; use SDL_stdinc_h;
with SDL_pixels_h; use SDL_pixels_h;
with Display.Basic.Fonts; use Display.Basic.Fonts;
with Display.Basic.GLFonts; use Display.Basic.GLFonts;
package body Display.Basic is

   Initialized : boolean := False with Atomic, Volatile;

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


   procedure Reshape (S : SDL_Window_Surface);
   procedure UpdateProjection(Canvas : Canvas_ID);


   ----------
   -- Draw --
   ----------

--     procedure Swap_Buffers(Window : Window_ID) is
--     begin
--        SDL_GL_SwapWindow (Stored_Windows(Window).window);
--     end Swap_Buffers;


   function Get_Zoom_Factor(Canvas : Canvas_ID) return Float is
   begin
      return  Get_Internal_Canvas(Canvas).Zoom_Factor;
   end Get_Zoom_Factor;

   procedure Set_Zoom_Factor (Canvas : Canvas_ID; ZF : Float) is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Display.Basic.Utils.Set_Zoom_Factor(Canvas, ZF);
    --  Reshape(Canvas, Integer(C.Surface.w), Integer(C.Surface.h));
      UpdateProjection(Canvas);
   end Set_Zoom_Factor;


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

--     type T_Internal_Canvas is record
--        Surface : access SDL_Surface;
--        Zoom_Factor : Float := 1.0;
--        Center : Screen_Point := (0, 0);
--     end record;
--
--     type Internal_Canvas_Array is array (Canvas_ID) of T_Internal_Canvas;
--
--     Internal_Canvas : Internal_Canvas_Array;
--     Nb_Canvas : Integer := 0;
--
--
--     function Register_SDL_Surface(S : access SDL_Surface) return Canvas_ID is
--        Current_Id : Canvas_ID;
--     begin
--        if Nb_Canvas = Internal_Canvas'Length then
--           raise Too_Many_Canvas;
--        end if;
--
--        Current_Id := Canvas_ID(Integer(Internal_Canvas'First) + Nb_Canvas);
--        Internal_Canvas(Current_Id) := T_Internal_Canvas'(Surface     => S,
--                                                          Zoom_Factor => 1.0,
--                                                          Center => (0, 0));
--        Nb_Canvas := Nb_Canvas + 1;
--        return Current_Id;
--     end Register_SDL_Surface;

   Quadric     : System.Address;
   procedure UpdateProjection(Canvas : Canvas_ID) is
      C : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
      S : access SDL_Surface := C.Surface;
      Z : double := 1.0 / double (C.Zoom_Factor);
   begin
      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;
      glOrtho (-GLdouble(S.w / 2) * Z, GLdouble(S.w / 2) * Z,
               -GLdouble(S.h / 2) * Z, GLdouble(S.h / 2) * Z, -100.0, 300.0);
      glMatrixMode (GL_MODELVIEW);
      glLoadIdentity;

   end UpdateProjection;

   procedure Reshape (S : SDL_Window_Surface) is
      C : T_Internal_Canvas := Get_Internal_Canvas(S.surface);
      Z : double := 1.0;--double(C.Zoom_Factor);
   begin
      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;

      glViewport (0, 0, GLsizei (S.w), GLsizei (S.h));
      glOrtho (-GLdouble(S.w / 2) * Z, GLdouble(S.w / 2) * Z,
               -GLdouble(S.h / 2) * Z, GLdouble(S.h / 2) * Z, -100.0, 300.0);

      --gluPerspective(70.0,GLdouble(Width)/GLdouble(Height),1.0,1000.0);

      --      glOrtho (-GLdouble(Width / 2), GLdouble(Width / 2), -GLdouble(Height / 2), GLdouble(Height / 2), -100.0, 300.0);
--        gluLookAt (0.0, 0.0, -100.0,
--                   0.0, 0.0, 0.0,
--                   0.0, 1.0, 0.0);
--        Ratio := GLdouble (w) / GLdouble (h);
--
--        if w > h then
--           glOrtho (-100.0 * Ratio, 100.0 * Ratio, -100.0, 100.0, -100.0, 300.0);
--        else
--           glOrtho (-100.0, 100.0, -100.0 / Ratio, 100.0 / Ratio, -100.0, 300.0);
--        end if;
     -- glOrtho (-GLdouble(Width / 2), GLdouble(Width / 2), -GLdouble(Height / 2), GLdouble(Height / 2), -100.0, 300.0);


      glMatrixMode (GL_MODELVIEW);
      glLoadIdentity;

      Update_SDL_Surface(S.surface, SDL_GetWindowSurface(S.window));

   end Reshape;

   function Init_GL (S : SDL_Window_Surface) return Boolean is
      glcontext : SDL_GLContext;
      type GLFloat_Array is array (Natural range <>) of aliased GLfloat;
      light_diffuse : aliased GLFloat_Array
        := (1.0, 1.0, 1.0, 1.0);
      light_ambient : aliased GLFloat_Array
        := (0.2, 0.2, 0.2, 1.0);
      light_position : aliased GLFloat_Array
        := (0.0, 0.0, 0.0, 1.0);
      GL_MULTISAMPLE : constant :=  16#809D#;

   begin

      if SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1) /= 0 then
          Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
       end if;
       if SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24) /= 0 then
          Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
       end if;

--        if SDL_GL_SetAttribute( SDL_GL_MULTISAMPLEBUFFERS, 1 ) /= 0 then
--           Ada.Text_IO.Put_Line ("impossible d'initialiser SDL_GL_MULTISAMPLEBUFFERS à 1");
--        elsif SDL_GL_SetAttribute( SDL_GL_MULTISAMPLESAMPLES, 6 ) /= 0 then
--            Ada.Text_IO.Put_Line ("impossible d'initialiser SDL_GL_MULTISAMPLESAMPLES sur 6 buffers");
--        end if;

      if SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1) /= 0 then
         Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
      end if;
      if SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 4) /= 0 then
         Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
      end if;


      glcontext := SDL_GL_CreateContext(S.window);

      if System.Address (glcontext) = Null_Address then
             Ada.Text_IO.Put_Line ("Problem in creating GL context");
      end if;



      Reshape (S);



--        glCullFace( GL_BACK );
--        glFrontFace( GL_CCW );
--        glEnable( GL_CULL_FACE );
--

      glClearDepth(1.0);
      glShadeModel(GL_SMOOTH);
      glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
      glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
      glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
      glEnable(GL_LINE_SMOOTH);
      glEnable(GL_POLYGON_SMOOTH);
      glEnable(GL_POINT_SMOOTH);
      glEnable(GL_MULTISAMPLE);
--           glDisable(GL_LINE_SMOOTH);
--        glDisable(GL_POLYGON_SMOOTH);
--        glDisable(GL_POINT_SMOOTH);
 --   glEnable(GL_BLEND);
   --   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
--  glFrontFace (GL_CW)

--  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
--        glEnable(GL_BLEND);
    --  glEnable(GL_POLYGON_SMOOTH);
--      glBlendFunc( GL_SRC_ALPHA_SATURATE, GL_ONE ) ;
      glEnable (GL_DEPTH_TEST);
    glEnable (GL_COLOR_MATERIAL);

--      glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
    -- lEnable (GL_NORMALIZE);
 --     glDepthFunc (GL_LESS);

--       glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);


       Quadric := gluNewQuadric;
      glEnable (GL_LIGHTING);
      glEnable (GL_LIGHT0);
      glLightfv (GL_LIGHT0, GL_AMBIENT, light_ambient (0)'Access);
      glLightfv (GL_LIGHT0, GL_DIFFUSE, light_diffuse (0)'Access);
      glLightfv (GL_LIGHT0, GL_POSITION, light_position (0)'Access);
   --  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,GL_TRUE);

      glClearColor (0.0, 0.0, 0.0, 1.0);
--glDisable(GL_CULL_FACE);


      --gluQuadricDrawStyle(Quadric, GLU_FILL);

      return True;
   end Init_GL;

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
--        glTranslated
--          (GLdouble (Position.X - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
--           GLdouble (Position.Y - (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
--           GLdouble (Position.Z));
      glTranslated
        (GLdouble ( - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
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

   function Create_SDL_Window (Width : Integer; Height : Integer; Name : String) return SDL_Window_Surface is
      S : SDL_Window_Surface;
     SDL_S : access SDL_Surface;
   begin
--        Ada.Text_IO.Put_Line("Create_SDL_Window Entry ");
      if not Initialized then
         raise Graphical_Context_Not_Initialized;
      end if;

      --  To center a non-fullscreen window we need to set an environment
      --  variable
      if SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2) /= 0 then
             Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
      end if;

      if SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1) /= 0 then
          Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
      end if;
      S.window := SDL_CreateWindow(New_String(Name), SDL_WINDOWPOS_CENTERED,
                                 SDL_WINDOWPOS_CENTERED,
                                 Interfaces.C.int(Width),
                                  Interfaces.C.int(Height),
                                   SDL_WINDOW_OPENGL or
                                     SDL_WINDOW_SHOWN or
                                  SDL_WINDOW_RESIZABLE);



      S.w := Width;
      S.h := Height;


        SDL_S := SDL_GetWindowSurface(S.window);

      if SDL_S = null then
         Put_Line ("Error retrieving the window surface");
         Put_Line(Value (SDL_GetError));
         SDL_Quit;
         raise Graphical_Context_Not_Initialized;
      end if;

      S.surface := Register_SDL_Surface(SDL_S);

      if not Init_GL(S) then
         Ada.Text_IO.Put_Line ("Error in Init_GL");
      end if;

      return S;

   end Create_SDL_Window;




   function Get_Canvas(Window : Window_ID) return Canvas_ID is
   begin
      -- as there is only one opengl canvas per applcation in an sdl opengl context a constant is returned
      return Canvas_ID'First;
   end Get_Canvas;

   procedure Set_Color(Color : RGBA_T) is
   begin
      glColor3d (double(Color.R) / 255.0,
                 double(Color.G) / 255.0,
                 double(Color.B) / 255.0);
   end Set_Color;

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
         Disable_3d_Light(Canvas);
         glBegin(GL_POINTS);
         --glColor3f(1,1,1);
         glVertex2i(0, 0);
         glEnd;
         Enable_3d_Light(Canvas);
      else
         gluSphere
           (qobj   => Quadric,
            radius => double(Radius),
            slices => 20,
            stacks => 20);
      end if;

      glPopMatrix;

   end Draw_Sphere;

   procedure Fill(Canvas : Canvas_ID; Color: RGBA_T) is null;
   procedure Draw_Sphere (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T) is null;

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

      gluDisk(Quadric, GLdouble(Radius), GLdouble(Radius + 1.0), 40, 40);


      Enable_3d_Light (Canvas);
      glPopMatrix;
   end Draw_Circle;

   procedure Draw_Circle (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T) is null;

   procedure Draw_Line (Canvas : Canvas_ID; P1: Point_3d; P2 : Point_3d; Color: RGBA_T) is null;
   procedure Draw_Line (Canvas : Canvas_ID; P1: Screen_Point; P2 : Screen_Point; Color: RGBA_T) is null;



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

   procedure Draw_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T) is null;

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
      null;
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

   procedure Set_Pixel (Canvas : Canvas_ID; Position : Screen_Point; Color : RGBA_T) is null;
   function Get_Text_Size(Text : String) return Screen_Point is  begin
      return String_Size (Font8x8, Text);
   end Get_Text_Size;

   function Scale (Canvas: T_Internal_Canvas; L : Float) return Integer is (Integer (L * Canvas.Zoom_Factor));

   procedure Set_Center (Canvas : Canvas_ID; Position : Point_3d)is
       C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Display.Basic.Utils.Set_Center(Canvas, (Scale(C, Position.X), Scale(C, Position.Y)));
   end Set_Center;

   procedure Set_Center (Canvas : Canvas_ID; Position : Screen_Point) is
   begin
       Display.Basic.Utils.Set_Center(Canvas, Position);
   end Set_Center;

   function Get_Center (Canvas : Canvas_ID) return Screen_Point  is
   begin
        return Display.Basic.Utils.Get_Center(Canvas);
   end Get_Center;

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
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      return Screen_Point'(Integer(IC.Surface.w), Integer(IC.Surface.h));
   end Get_Canvas_Size;

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

            when SDL_events_h.SDL_WINDOWEVENT_Evt =>
               case E.window.event is
                  when SDL_WindowEventID'Pos(SDL_WINDOWEVENT_RESIZED) |
                       SDL_WindowEventID'Pos(SDL_WINDOWEVENT_SIZE_CHANGED) =>
                     Stored_Windows(Window_ID'First).w := Integer(E.window.data1);
                     Stored_Windows(Window_ID'First).h := Integer(E.window.data2);
                     Reshape (Stored_Windows(Window_ID'First));
                  when others =>
                     null;
               end case;

            when others =>
               null;
         end case;
      end loop;
   end Poll_Events;

   procedure Swap_Buffers(Window : Window_ID; Erase : Boolean := True)is
   begin



      glFlush;
      if Stored_Windows(Window).window = null then
         Ada.Text_IO.Put_Line ("Error!!!");
      end if;
      SDL_GL_SwapWindow(Stored_Windows(Window).window);
--SDL_GL_SwapBuffers;
     -- SDL_Delay (1);
      glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  --    Poll_Events(S);


--        SDL_GL_SwapWindow(window);
--        if SDL_UpdateWindowSurface (Stored_Windows(Window).window) < 0 then
--           raise Display_Error;
--        end if;
--        if Erase then
--           if SDL_FillRect (Canvas.Surface, null, 0) < 0 then
--              raise Display_Error;
--           end if;
--        end if;
      Poll_Events;

   end Swap_Buffers;
   --  Update the canvas

   -- copy the hidden buffer to the visible buffer
   procedure Swap_Copy_Buffers(Window : Window_ID)is null;
   --  Update the canvas

   function Get_Cursor_Status return Cursor_T is (Internal_Cursor);



   procedure Init is
   begin
      --  SDL is comprised of 8 subsystems. Here we initialize the video
      if SDL_Init(SDL_INIT_VIDEO) < 0 then
         Put_Line ("Error initializing SDL");
         Put_Line(Value (SDL_GetError));
         SDL_Quit;
      end if;



      Initialized := True;
   end Init;


begin

   Init;


end Display.Basic;
