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

package Display.Basic is


   type Window_ID is private;

   function Create_Window (Width : Integer; Height : Integer; Name : String) return Window_ID;

   Graphical_Context_Not_Initialized : exception;
   Too_Many_Windows: exception;
   Too_Many_Canvas : exception;

   type Canvas_ID is  private;

   function Get_Canvas(Window : Window_ID) return Canvas_ID;

   type Point_2d is record
      X : Float;
      Y : Float;
   end record;

   type Point_3d is record
      X : Float;
      Y : Float;
      Z : Float;
   end record;

   type Screen_Point is record
      X, Y : Integer;
   end record;

   type Cursor_T is record
      Position : Screen_Point;
      Pressed : Boolean;
   end record;

   procedure Fill(Canvas : Canvas_ID; Color: RGBA_T);
   procedure Draw_Sphere (Canvas : Canvas_ID; Position: Point_3d; Radius : Float; Color: RGBA_T);
   procedure Draw_Sphere (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T);
   procedure Draw_Circle (Canvas : Canvas_ID; Position: Point_3d; Radius : Float; Color: RGBA_T);
   procedure Draw_Circle (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T);
   procedure Draw_Line (Canvas : Canvas_ID; P1: Point_3d; P2 : Point_3d; Color: RGBA_T);
   procedure Draw_Line (Canvas : Canvas_ID; P1: Screen_Point; P2 : Screen_Point; Color: RGBA_T);
   procedure Draw_Text (Canvas : Canvas_ID; Position: Point_3d; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True);
   procedure Draw_Text (Canvas : Canvas_ID; Position: Screen_Point; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True);
   procedure Draw_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T);
   procedure Draw_Rect (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T);
   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T);
   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T);
   procedure Set_Pixel (Canvas : Canvas_ID; Position : Screen_Point; Color : RGBA_T);

   procedure Enable_3d_Light (Canvas : Canvas_ID);
   procedure Disable_3d_Light (Canvas : Canvas_ID);
   procedure Set_3d_Light (Canvas : Canvas_ID;
                           Position : Point_3d;
                           Diffuse_Color : RGBA_T;
                           Ambient_Color : RGBA_T);



   function Get_Text_Size(Text : String) return Screen_Point;

   function Get_Zoom_Factor (Canvas : Canvas_ID) return Float;
   procedure Set_Zoom_Factor (Canvas : Canvas_ID; ZF : Float);

   procedure Set_Center (Canvas : Canvas_ID; Position : Point_3d);
   procedure Set_Center (Canvas : Canvas_ID; Position : Screen_Point);
   function Get_Center (Canvas : Canvas_ID) return Screen_Point;
   function Get_Canvas_Size(Canvas : Canvas_ID) return Screen_Point;
   procedure Swap_Buffers(Window : Window_ID; Erase : Boolean := True);
   --  Update the canvas

   -- copy the hidden buffer to the visible buffer
   procedure Swap_Copy_Buffers(Window : Window_ID);
   --  Update the canvas

   function Get_Cursor_Status return Cursor_T;

   function To_Point3d (Canvas : Canvas_ID; P : Screen_Point) return Point_3d;
   function To_Screen_Point (Canvas : Canvas_ID;  P : Point_3d) return Screen_Point;

  function Is_Killed return Boolean;


private


   type Window_ID_Range is range 0 .. 10;
   type Window_ID is new Window_ID_Range;

   type Canvas_ID_Range is range 1 .. 1024;
   type Canvas_ID is new Canvas_ID_Range;


end Display.Basic;
