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

with GNAT.Strings; use GNAT.Strings;
with GL_Gl_H;      use GL_Gl_H;
with Interfaces.C; use Interfaces.C;
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with SDL_SDL_video_h; use SDL_SDL_video_h;
with Display.Basic; use Display.Basic;

private package Display.Kernel is

   --  This kernel is managing two sets of data. The protected object
   --  Data_Manager is responsible to manage a protected set of objects. It
   --  contains an internal representation of the graphical context, and
   --  buffers the sequence of commands that issued this representation since
   --  the last flush performed by the graphical loop. The graphical loops
   --  contains its own representation of the context (see Shapes in the body),
   --  which is only updated once at each iteration.



   type Id_Stack is array (Integer range <>) of Natural;

   --  This type offers a thread-safe interface to the graphical data. Commands
   --  are taken into account immediately by its internal state. However,
   --  the graphical loop will only read those commands once per cycle.


   type E_Pixel is record
      R : GLubyte;
      G : GLubyte;
      B : GLubyte;
      A : GLubyte;
   end record;
   pragma Convention (C, E_Pixel);

   type Pixel_Array is array (int range <>) of E_Pixel;
   pragma Convention (C, Pixel_Array);

   type Pixel_Array_Access is access all Pixel_Array;



   At_End : Boolean := False
     with Atomic;


   type OpenGL_Surface is record
      Canvas : Canvas_ID;
      surface : access SDL_Surface;
      vidInfo : access SDL_VideoInfo;
      w       : Integer := 400;
      h       : Integer := 400;
      bpp   :  Interfaces.C.int := 16;
      flags :  Interfaces.C.unsigned := SDL_OPENGL + SDL_HWSURFACE + SDL_RESIZABLE;
   end record;
   procedure UpdateProjection(Canvas : Canvas_ID);

   procedure Swap_Buffers(S : in out OpenGL_Surface);

   procedure Poll_Events(S : in out OpenGL_Surface);

   procedure Reshape (S : in out OpenGL_Surface; W : Integer; H : Integer);

   function Create_Window(Width:Integer; Height : Integer; Name : String) return OpenGL_Surface;

   procedure Draw_Sphere (Radius:Float; Color: RGBA_T);

   procedure DrawDisk (InnerRadius : Float; OuterRadius : Float; VSlices : Integer; HSlices : Integer; Color: RGBA_T);

   function Is_Stopped return Boolean;

   function Get_Internal_Cursor return Cursor_T;

end Display.Kernel;
