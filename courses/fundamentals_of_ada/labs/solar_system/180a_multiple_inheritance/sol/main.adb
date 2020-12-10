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

with Ada.Real_Time;                     use Ada.Real_Time;
with Display;                           use Display;


with Display.Basic;                     use Display.Basic;
with Solar_System;                      use Solar_System;
with Solar_System.Graphics; use Solar_System.Graphics;
procedure Main is

   S : constant access Visible_Solar_System_T := Create_Visible(Create_Solar_System);

   -- declare a variable Next of type Time to store the Next step time
   Next : Time;

   -- declare a constant Period of 40 milliseconds of type Time_Span defining the loop period
   Period  : constant Time_Span := Milliseconds (40);

    -- reference to the application window
   Window : Window_ID;

   -- reference to the graphical canvas associated with the application window
   Canvas : Canvas_ID;

   Sun : access Still_Body_I'Class;
   Black_Hole : access Orbiting_Body_I'Class;

begin

   -- Create the main window
   Window := Create_Window(Width  => 240,
                           Height => 320,
                           Name   => "Solar System");
   -- retrieve the graphical canvas associated with the main window
   Canvas := Get_Canvas (Window);

   Sun := Create_Visible(Create_Still(0.0, 0.0), 20.0, Yellow);

   S.Add_Still_Body(Sun);

   S.Add_Moving_Body(Create_Visible(B => Create_Orbiting(Distance => 50.0,
                                                         Speed => 0.02,
                                                         Angle => 0.0,
                                                         Turns_Around => Sun),

                                    Radius => 5.0,
                                    Color  => Blue));

   Black_Hole := Create_Orbiting(Distance     => 70.0,
                                 Speed        => 0.01,
                                 Angle        => 0.0,
                                 Turns_Around => Sun);
   S.Add_Moving_Body(Black_Hole);

   S.Add_Moving_Body(Create_Visible(B => Create_Orbiting(Distance     => 8.0,
                                                         Speed        => 0.1,
                                                         Angle        => 0.0,
                                                         Turns_Around => Black_Hole),
                                    Radius => 1.0,
                                    Color  => Red));

   S.Add_Moving_Body(Create_Visible(B => Create_Orbiting(Distance     => 12.0,
                                                         Speed        => -0.1,
                                                         Angle        => 0.0,
                                                         Turns_Around => Black_Hole),
                                    Radius => 1.0,
                                    Color  => Red));


     --  initialize the Next step time begin the current time (Clock) + the period
   Next := Clock + Period;

   --create an infinite loop
   --  call Move_All procedure
   --  call Draw_All procedure
   --  call Swap_Buffers to update the screen
   --  wait until Next time
   --  update the Next time
   while not Is_Killed loop

      S.Move;
      S.Draw (Canvas);
      Display.Basic.Swap_Buffers(Window);

      delay until Next;
      Next := Next + Period;
   end loop;

end Main;
