--  -----------------------------------------------------------------------
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

with Interfaces.C.Strings;  use Interfaces.C.Strings;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with System;                            use System;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Calendar;   use Ada.Calendar;

with GL_glu_h;         use GL_glu_h;
with SDL_SDL_h;        use SDL_SDL_h;
with SDL_SDL_stdinc_h; use SDL_SDL_stdinc_h;
with SDL_SDL_video_h;  use SDL_SDL_video_h;
with SDL_SDL_events_h; use SDL_SDL_events_h;
with SDL_SDL_timer_h;  use SDL_SDL_timer_h;
with SDL_SDL_keysym_h; use SDL_SDL_keysym_h;
with SDL_SDL_ttf_h;    use SDL_SDL_ttf_h;

with Ada.Unchecked_Deallocation;
with Ada.Task_Identification;
with Ada.Task_Termination;    use Ada.Task_Termination;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with SDL_SDL_error_h; use SDL_SDL_error_h;
with Display.Basic.Utils; use Display.Basic.Utils;
package body Display.Kernel is


   Initialized : Boolean := False with Atomic, Volatile;
   Quadric     : System.Address;

  -- Window_Width, Window_Height : Integer;

   --  a shared variable, set concurrently by the Poll_Events routine and read
   --  by client code


   -----------
   -- Check --
   -----------

   procedure Check (Ret : Int) is
   begin
      if Ret /= 0 then
         raise Display_Error;
      end if;
   end Check;


   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color (Color : RGBA_T) is
   begin
      glColor3d (double(Color.R) / 255.0 ,
                 double(Color.G) / 255.0,
                 double(Color.B) / 255.0);
   end Set_Color;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (Length : Float; Width : Float) is
   begin
      glBegin(GL_QUADS);

      -- Front Face
      glNormal3f (0.0, 0.0, 1.0);
      glVertex3f (0.0, -Width, Width);
      glVertex3f (Length, -Width, Width);
      glVertex3f (Length, Width, Width);
      glVertex3f (0.0, Width, Width);

      -- Back Face
      glNormal3f (0.0, 0.0, -1.0);
      glVertex3f (0.0, -Width, -Width);
      glVertex3f (0.0, Width, -Width);
      glVertex3f (Length, Width, -Width);
      glVertex3f (Length, -Width, -Width);

      -- Top Face
      glNormal3f (0.0, 1.0, 0.0);
      glVertex3f (0.0, Width, -Width);
      glVertex3f (0.0, Width, Width);
      glVertex3f (Length, Width, Width);
      glVertex3f (Length, Width, -Width);

      -- Bottom Face
      glNormal3f (0.0, -1.0, 0.0);
      glVertex3f (0.0, -Width, -Width);
      glVertex3f (Length, -Width, -Width);
      glVertex3f (Length, -Width, Width);
      glVertex3f (0.0, -Width, Width);

      -- Right face
      glNormal3f(1.0, 0.0, 0.0);
      glVertex3f (Length, -Width, -Width);
      glVertex3f (Length, Width, -Width);
      glVertex3f (Length, Width, Width);
      glVertex3f (Length, -Width, Width);

      -- Left Face
      glNormal3f (-1.0, 0.0, 0.0);
      glVertex3f (0.0, -Length, -Length);
      glVertex3f (0.0, -Length, Length);
      glVertex3f (0.0, Length, Length);
      glVertex3f (0.0, Length, -Length);

      glEnd;
   end Draw_Line;

   --------------
   -- Draw_Box --
   --------------

   procedure Draw_Box (X, Y, Width, Height : Float) is
      pragma Unreferenced (X, Y);
      X1 : constant Float := -Width / 2.0;
      Y1 : constant Float := -Height / 2.0;
      X2 : constant Float := -Width / 2.0;
      Y2 : constant Float := Height / 2.0;
      X3 : constant Float := Width / 2.0;
      Y3 : constant Float := Height / 2.0;
      X4 : constant Float := Width / 2.0;
      Y4 : constant Float := -Height / 2.0;

      Dx : constant := 2.0;
      Dy : constant := 2.0;
      Back : constant := -20.0;
      Front : constant := 0.0;
   begin
      glBegin(GL_QUADS);

      glNormal3f(-1.0, 0.0, 0.0);
      glVertex3f(X1, Y1, Front);
      glVertex3f(X2, Y2, Front);
      glVertex3f(X2 - Dx, Y2 - Dy, Back);
      glVertex3f(X1 - Dx, Y1 - Dy, Back);

      glNormal3f(1.0, 0.0, 0.0);
      glVertex3f(X3, Y3, Front);
      glVertex3f(X4, Y4, Front);
      glVertex3f(X4 - Dx, Y4 - Dy, Back);
      glVertex3f(X3 - Dx, Y3 - Dy, Back);

      glNormal3f(0.0, 0.0, 1.0);
      glVertex3f(X1, Y1, Front);
      glVertex3f(X2, Y2, Front);
      glVertex3f(X3, Y3, Front);
      glVertex3f(X4, Y4, Front);

      glNormal3f(0.0, 0.0, -1.0);
      glVertex3f(X1 - Dx, Y1 - Dy, Back);
      glVertex3f(X2 - Dx, Y2 - Dy, Back);
      glVertex3f(X3 - Dx, Y3 - Dy, Back);
      glVertex3f(X4 - Dx, Y4 - Dy, Back);

      glNormal3f(0.0, 1.0, 0.0);
      glVertex3f(X2, Y2, Front);
      glVertex3f(X3, Y3, Front);
      glVertex3f(X3 - Dx, Y3 - Dy, Back);
      glVertex3f(X2 - Dx, Y2 - Dy, Back);

      glNormal3f(0.0, -1.0, 0.0);
      glVertex3f(X4, Y4, Front);
      glVertex3f(X1, Y1, Front);
      glVertex3f(X1 - Dx, Y1 - Dy, Back);
      glVertex3f(X4 - Dx, Y4 - Dy, Back);

      glEnd;
   end Draw_Box;

   ----------------
   -- Draw_Torus --
   ----------------

   procedure Draw_Torus
     (Inner_Radius : GLfloat;
      Outer_Radius : GLfloat;
      Nsides : GLint;
      Rings : GLint)
   is
      Theta, Phi, Theta1 : GLfloat;
      CosTheta, SinTheta : GLfloat;
      CosTheta1, SinTheta1 : GLfloat;
      RingDelta, SideDelta : GLfloat;
   begin
      RingDelta := 2.0 * Pi / GLfloat (Rings);
      SideDelta := 2.0 * Pi / GLfloat (Nsides);

      Theta := 0.0;
      CosTheta := 1.0;
      SinTheta := 0.0;

      for i in reverse 0 .. Rings - 1 loop
         Theta1 := Theta + RingDelta;
         CosTheta1 := cos(Theta1);
         SinTheta1 := sin(Theta1);
         glBegin(GL_QUAD_STRIP);
         phi := 0.0;
         for j in reverse 0 .. nsides loop
            declare
               CosPhi, SinPhi, Dist : GLfloat;
            begin
               Phi := Phi + SideDelta;
               CosPhi := Cos (Phi);
               SinPhi := Sin (Phi);
               Dist := Outer_Radius + Inner_Radius * CosPhi;

               glNormal3f
                 (cosTheta1 * CosPhi, -SinTheta1 * CosPhi, SinPhi);
               glVertex3f
                 (cosTheta1 * dist, -SinTheta1 * dist, Inner_Radius * SinPhi);
               glNormal3f
                 (cosTheta * CosPhi, -SinTheta * CosPhi, sinPhi);
               glVertex3f
                 (cosTheta * Dist, -SinTheta * Dist,  Inner_Radius * SinPhi);
            end;
         end loop;
         glEnd;

         Theta := Theta1;
         CosTheta := CosTheta1;
         SinTheta := SinTheta1;
      end loop;
   end Draw_Torus;

   ----------
   -- Draw --
   ----------
   procedure DrawDisk (InnerRadius : Float; OuterRadius : Float; VSlices : Integer; HSlices : Integer; Color: RGBA_T) is
   begin
       gluDisk(Quadric, GLdouble(InnerRadius), GLdouble(OuterRadius), int(VSlices), int(HSlices));
   end DrawDisk;

   procedure Draw_Sphere (Radius : Float; Color: RGBA_T) is
   begin

      gluSphere
        (qobj   => Quadric,
         radius => GLdouble (Radius),
         slices => 20,
         stacks => 20);

   end Draw_Sphere;

   --------------------
   -- Graphical loop --
   --------------------
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

   procedure Reshape (S : in out OpenGL_Surface; W : Integer; H : Integer) is
      C : T_Internal_Canvas := Get_Internal_Canvas (S.Canvas);
      SDL_S : access SDL_Surface := C.Surface;
   begin
      S.w := W;
      S.h := H;
      SDL_S.w := int(W);
      SDL_S.h := int(H);

      glViewport (0, 0, GLsizei (w), GLsizei (h));
      UpdateProjection(S.Canvas);

     -- glMatrixMode (GL_PROJECTION);
      --glLoadIdentity;

--        Ratio := GLdouble (w) / GLdouble (h);
--
--        if w > h then
--           glOrtho (-100.0 * Ratio, 100.0 * Ratio, -100.0, 100.0, -100.0, 300.0);
--        else
--           glOrtho (-100.0, 100.0, -100.0 / Ratio, 100.0 / Ratio, -100.0, 300.0);
--        end if;
   --   glOrtho (-GLdouble(S.w / 2), GLdouble(S.w / 2), -GLdouble(S.h / 2), GLdouble(S.h / 2), -100.0, 300.0);

 --     glMatrixMode (GL_MODELVIEW);

   end Reshape;

   Stop : Boolean := False;

   function Set_SDL_Video (Width : Integer; Height : Integer) return OpenGL_Surface;
   procedure Set_OpenGL(S : in out OpenGL_Surface);

   type Glubyte_Arrays is array (int range <>) of aliased GLubyte;




   -------------
   -- GL_Task --
   -------------

   function Create_Window(Width:Integer; Height : Integer; Name : String) return OpenGL_Surface is
      Window: OpenGL_Surface;

   begin
      if not Initialized then
         raise Use_Error;
      end if;

      --  Rather than set the video properties up in the constructor, I set
      --  them in setVideo. The reason for this is that 2 pointers are used to
      --  interact with SDL structures. Once used they convert their handles
      --  into vidInfo and surface tamer variables. That this occurs inside
      --  the function means the pointers will release their memory on function
      --  exit.

      Window := Set_SDL_Video (Width, Height);
      Window.Canvas := Register_SDL_Surface(Window.surface);

      SDL_WM_SetCaption (Title => New_String (Name), Icon  => Null_Ptr);

      --  openGL is not part of SDL, rather it runs in a window handled
      --  by SDL. here we set up some openGL state

      Set_OpenGL(Window);
      glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      Check (TTF_Init);
      return Window;
   end Create_Window;

   procedure Init is
   begin
      --  SDL is comprised of 8 subsystems. Here we initialize the video
      if SDL_Init(SDL_INIT_VIDEO) < 0 then
         Put_Line ("Error initializing SDL");
         Put_Line(Value (SDL_GetError));
         SDL_SDL_h.SDL_Quit;
      end if;



      Initialized := True;
   end Init;

   ----------
   -- Draw --
   ----------

   procedure Swap_Buffers(S : in out OpenGL_Surface) is
   begin
     -- Idle;
   --   Poll_Events;

      glFlush;
      SDL_GL_SwapBuffers;
--      SDL_Delay (1);
      glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      Poll_Events(S);

   end Swap_Buffers;

   ------------
   -- Finish --
   ------------

   procedure Finish is
   begin
      TTF_Quit;
      SDL_SDL_h.SDL_Quit;
      GNAT.OS_Lib.OS_Exit (0);
   end Finish;

   -------------------
   -- Set_SDL_Video --
   -------------------

   function Set_SDL_Video(Width : Integer; Height : Integer) return OpenGL_Surface is
      S : OpenGL_Surface;
      ret : int;
   begin
      --  To center a non-fullscreen window we need to set an environment
      --  variable

      Check (SDL_putenv(New_String ("SDL_VIDEO_CENTERED=center")));

      --  the video info structure contains the current video mode. Prior to
      --  calling setVideoMode, it contains the best available mode
      --  for your system. Post setting the video mode, it contains
      --  whatever values you set the video mode with.
      --  First we point at the SDL structure, then test to see that the
      --  point is right. Then we copy the data from the structure to
      --  the safer vidInfo variable.

      declare
         ptr  : System.Address := SDL_GetVideoInfo;
         for ptr'Address use S.vidInfo'Address;
      begin
         if ptr = System.Null_Address then
            Put_Line ("Error querying video info");
            Put_Line(Value (SDL_GetError));
            SDL_SDL_h.SDL_Quit;
         end if;
      end;

      S.w := Width;
      S.h := Height;
      ret := SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
      ret := SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

      ret := SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1);
      ret := SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 4);
      --  the setVideoMode function returns the current frame buffer as an
      --  SDL_Surface. Again, we grab a pointer to it, then place its
      --  content into the non pointery surface variable. I say 'non-pointery',
      --  but this SDL variable must have a pointer in it because it can
      --  access the current pixels in the framebuffer.

      S.surface := SDL_SetVideoMode(int (S.w), int (S.h), S.bpp, S.flags);

      if S.surface = null then
         Put_Line ("Error setting the video mode");
         Put_Line(Value (SDL_GetError));
         SDL_SDL_h.SDL_Quit;
      end if;

      return S;
   end Set_SDL_Video;

   ----------------
   -- Set_OpenGL --
   ----------------

   procedure Set_OpenGL(S : in out OpenGL_Surface) is
      type GLFloat_Array is array (Natural range <>) of aliased GLfloat;
      light_diffuse : aliased GLFloat_Array
        := (1.0, 1.0, 1.0, 1.0);
      light_ambient : aliased GLFloat_Array
        := (0.2, 0.2, 0.2, 1.0);
      light_position : aliased GLFloat_Array
        := (0.0, 0.0, 0.0, 1.0);
      GL_MULTISAMPLE : constant :=  16#809D#;
   begin
      Quadric := gluNewQuadric;
      Reshape (S, S.w, S.h);

      glClearColor (0.1, 0.1, 0.1, 0.0);
      glLightfv (GL_LIGHT0, GL_AMBIENT, light_ambient (0)'Access);
      glLightfv (GL_LIGHT0, GL_DIFFUSE, light_diffuse (0)'Access);
      glLightfv (GL_LIGHT0, GL_POSITION, light_position (0)'Access);
      glEnable(GL_MULTISAMPLE);
      glFrontFace (GL_CW);
      glEnable (GL_LIGHTING);
      glEnable (GL_LIGHT0);
      glEnable (GL_DEPTH_TEST);
      glEnable (GL_COLOR_MATERIAL);
      glEnable (GL_NORMALIZE);
      glDepthFunc (GL_LESS);

      glMatrixMode (GL_MODELVIEW);
--        gluLookAt (0.0, 0.0, 10.0,
--                   0.0, 0.0, 0.0,
--                   0.0, 1.0, 0.0);
   end Set_OpenGL;

   -----------------
   -- Poll_Events --
   -----------------
    Internal_Cursor : Cursor_T := ((0,0), False);

   function Get_Internal_Cursor return Cursor_T is (Internal_Cursor);


   procedure Poll_Events(S : in out OpenGL_Surface) is
      E : aliased SDL_Event;
   begin
      while SDL_PollEvent (E'Access) /= 0 loop
         case unsigned (E.c_type) is
            when SDL_SDL_events_h.SDL_QUIT =>
               Stop := True;
            when SDL_SDL_events_h.SDL_MOUSEBUTTONDOWN =>
               Internal_Cursor.Position := (Integer(E.motion.x), Integer(E.motion.y));
               Internal_Cursor.Pressed := True;
            when SDL_SDL_events_h.SDL_MOUSEBUTTONUP =>
               Internal_Cursor.Position := (Integer(E.motion.x), Integer(E.motion.y));
               Internal_Cursor.Pressed := False;

            when SDL_SDL_events_h.SDL_VIDEORESIZE =>
                Reshape (S, Integer (E.resize.w), Integer (E.resize.h));

            when others =>
               null;
         end case;
      end loop;
   end Poll_Events;
   -------------------------
   -- Exception_Reporting --
   -------------------------

   protected Exception_Reporting is
      procedure Report
        (Cause : Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence);
   end Exception_Reporting;

   -------------------------
   -- Exception_Reporting --
   -------------------------

   protected body Exception_Reporting is

      procedure Report
        (Cause : Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence) is
         pragma Unreferenced (Cause, T);
      begin
         Put_Line ("=== UNCAUGHT EXCEPTION ===");
         Put_Line (Exception_Information (X));

         Put_Line (Symbolic_Traceback (X));

         GNAT.OS_Lib.OS_Exit (1);
      end Report;

   end Exception_Reporting;

   function Is_Stopped return Boolean is
   begin
      if Stop then
         TTF_Quit;
         SDL_SDL_h.SDL_Quit;
        -- GNAT.OS_Lib.OS_Exit (0);
      end if;
      return Stop;
   end Is_Stopped;

begin
--   Data_Manager.Initialize;
--     Set_Dependents_Fallback_Handler (Exception_Reporting.Report'Access);
--     Set_Specific_Handler
--       (Ada.Task_Identification.Current_Task,
--        Exception_Reporting.Report'Access);
   Init;
end Display.Kernel;
