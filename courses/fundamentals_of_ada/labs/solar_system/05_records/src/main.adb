-----------------------------------------------------------------------
--                              Ada Labs                             --
--                                                                   --
--                 Copyright (C) 2008-2012, AdaCore                  --
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

with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with Display;       use Display;
with Display.Basic; use Display.Basic;
with Libm_Single;   use Libm_Single;

procedure Main is

   --  define type Bodies_Enum_T as an enumeration of Sun, Earth, Moon, Satellite
   type Bodies_Enum_T is (Sun, Earth, Moon, Satellite);

   --  define a type Body_T to store every information about a body
   --   X, Y, Distance, Speed, Angle, Color type is RGBA_T, Radius

   --  define type Bodies_Array_T as an array of Body_T indexed by bodies

   --  declare variable Bodies which is an array of Body_T

   --  declare a variable Next of type Time to store the Next step time
   Next : Time;

   --  declare a constant Period of 40 milliseconds of type Time_Span defining the loop period
   Period : constant Time_Span := Milliseconds (40);

   --  reference to the application window
   Window : Window_ID;

   --  reference to the graphical canvas associated with the application window
   Canvas : Canvas_ID;

begin

   --  Create a window 240x320
   Window :=
     Create_Window (Width => 240, Height => 320, Name => "Solar System");

   --  Retrieve the graphical canvas from the window
   Canvas := Get_Canvas (Window);

   --  initialize Bodies variable with parameters for each body using an aggregate
   --    Sun Distance = 0.0, Angle = 0.0, Speed = 0.0, Radius = 20.0, Color = Yellow
   --    Earth Distance = 50.0, Angle = 0.0, Speed = 0.02, Radius = 5.0, Color = Blue
   --    Moon Distance = 15.0, Angle = 0.0, Speed = 0.04, Radius = 2.0, Color = White
   --    Satellite Distance = 8.0, Angle = 0.0, Speed = 0.1, Radius = 1.0, Color = Red

   --  initialize the Next step time begin the current time (Clock) + the period
   Next := Clock + Period;

   while not Is_Killed loop

      --  create a loop to update each body position and angles
      --    the position of an object around (0,0) at distance d with an angle a
      --    is (d*cos(a), d*sin(a))
      --  update angle parameter of each body adding speed to the previous angle

      --  create a loop to draw every objects
      --  use the Draw_Sphere procedure to do it

      --  update the screen using procedure Swap_Buffers
      Swap_Buffers (Window);

      --  wait until Next
      delay until Next;

      --  update the Next time adding the period for the next step
      Next := Next + Period;

   end loop;

end Main;
