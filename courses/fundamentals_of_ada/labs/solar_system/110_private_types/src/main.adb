-----------------------------------------------------------------------
--                              Ada Labs                             --
--                                                                   --
--                 Copyright (C) 2008-2009, AdaCore                  --
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

with Ada.Real_Time; use Ada.Real_Time;

with Solar_System;          use Solar_System;
with Display;               use Display;
with Display.Basic;         use Display.Basic;
with Solar_System.Graphics; use Solar_System.Graphics;

procedure Main is

   --  declare variable Bodies which is an array of Body_T
   Bodies : Bodies_Array_T;

   --  declare a variable Next of type Time to store the Next step time
   Next : Time;

   --  declare a constant Period of 40 milliseconds of type Time_Span defining the loop period
   Period : constant Time_Span := Milliseconds (40);

   --  reference to the application window
   Window : Window_ID;

   --  reference to the graphical canvas associated with the application window
   Canvas : Canvas_ID;

begin

   --  Create the main window
   Window :=
     Create_Window (Width => 240, Height => 320, Name => "Solar System");
   --  retrieve the graphical canvas associated with the main window
   Canvas := Get_Canvas (Window);

   --  initialize Bodies variable with parameters for each body using an aggregate
   --    Sun Distance = 0.0, Angle = 0.0, Speed = 0.0, Radius = 20.0, Color = Yellow
   --    Earth Distance = 50.0, Angle = 0.0, Speed = 0.02, Radius = 5.0, Color = Blue
   --    Moon Distance = 15.0, Angle = 0.0, Speed = 0.04, Radius = 2.0, Color = White
   --    Satellite Distance = 8.0, Angle = 0.0, Speed = 0.1, Radius = 1.0, Color = Red
   Bodies :=
     (Sun =>
        (Distance     => 0.0,
         Angle        => 0.0,
         Speed        => 0.0,
         Radius       => 20.0,
         Color        => Yellow,
         Turns_Around => Sun,
         others       => <>),
      Earth =>
        (Distance     => 50.0,
         Angle        => 0.0,
         Speed        => 0.02,
         Radius       => 5.0,
         Color        => Blue,
         Turns_Around => Sun,
         others       => <>),
      Moon =>
        (Distance     => 15.0,
         Angle        => 0.0,
         Speed        => 0.04,
         Radius       => 2.0,
         Color        => White,
         Turns_Around => Earth,
         others       => <>),
      Satellite =>
        (Distance     => 8.0,
         Angle        => 0.0,
         Speed        => 0.1,
         Radius       => 1.0,
         Color        => Red,
         Turns_Around => Earth,
         others       => <>),
      Comet =>
        (Distance     => 80.0,
         Angle        => 0.0,
         Speed        => 0.05,
         Radius       => 1.0,
         Color        => Yellow,
         Turns_Around => Sun,
         others       => <>),
      Black_Hole =>
        (Distance     => 75.0,
         Angle        => 0.0,
         Speed        => 0.02,
         Turns_Around => Sun,
         Visible      => False,
         others       => <>),
      Asteroid_1 =>
        (Distance     => 5.0,
         Angle        => 0.0,
         Speed        => 0.1,
         Radius       => 1.0,
         Color        => Green,
         Turns_Around => Black_Hole,
         others       => <>),
      Asteroid_2 =>
        (Distance     => 5.0,
         Angle        => 3.14,
         Speed        => 0.1,
         Radius       => 1.0,
         Color        => Cyan,
         Turns_Around => Black_Hole,
         others       => <>));

   --  initialize the Next step time begin the current time (Clock) + the period
   Next := Clock + Period;

   --  create an infinite loop
   --  call Move_All procedure
   --  call Draw_All procedure
   --  call Swap_Buffers to update the screen
   --  wait until Next time
   --  update the Next time
   while not Is_Killed loop

      Move_All (Bodies);
      Draw_All (Bodies, Canvas);

      --  commented out because now useless : Anthony 10/01/2015
      --      Bodies (Sun).X := 200.0;
      Bodies (Sun).Y := 200.0;
      --        for B in Bodies_Enum loop
      --           Bodies (B).X := 10.0;
      --        end loop;
      --

      Display.Basic.Swap_Buffers (Window);

      delay until Next;
      Next := Next + Period;
   end loop;

end Main;
