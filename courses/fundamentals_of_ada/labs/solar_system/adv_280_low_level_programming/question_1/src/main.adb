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
--with Last_Chance_Handler;

procedure Main is

   --  declare a variable Now of type Time to record current time
   Now : Time;

   --  declare a constant Period of 40 milliseconds of type Time_Span defining the loop period
   Period : constant Time_Span := Milliseconds (40);

   --  reference to the application window
   Window : Window_ID;

begin

   --  Create the main window
   Window :=
     Create_Window (Width => 240, Height => 320, Name => "Solar System");

   Graphic_Context.Set_Window (Window);

   --  initialize Bodies using Init_Body procedure
   Init_Body
     (B            => Sun,
      Radius       => 20.0,
      Color        => Yellow,
      Distance     => 0.0,
      Speed        => 0.0,
      Turns_Around => Sun);

   Init_Body
     (B            => Earth,
      Radius       => 5.0,
      Color        => Blue,
      Distance     => 50.0,
      Speed        => 0.02,
      Turns_Around => Sun);

   Init_Body
     (B            => Moon,
      Radius       => 2.0,
      Color        => Gray,
      Distance     => 15.0,
      Speed        => 0.04,
      Turns_Around => Earth);

   Init_Body
     (B            => Satellite,
      Radius       => 1.0,
      Color        => Red,
      Distance     => 8.0,
      Speed        => 0.1,
      Turns_Around => Earth);

   Init_Body
     (B            => Comet,
      Radius       => 1.0,
      Color        => Yellow,
      Distance     => 80.0,
      Speed        => 0.05,
      Tail         => True,
      Turns_Around => Sun);

   Init_Body
     (B            => Black_Hole,
      Radius       => 0.0,
      Color        => Blue,
      Distance     => 75.0,
      Speed        => -0.02,
      Turns_Around => Sun,
      Visible      => False);

   Init_Body
     (B            => Asteroid_1,
      Radius       => 2.0,
      Color        => Green,
      Distance     => 5.0,
      Speed        => 0.1,
      Turns_Around => Black_Hole);

   Init_Body
     (B            => Asteroid_2,
      Radius       => 2.0,
      Color        => Blue,
      Distance     => 5.0,
      Speed        => 0.1,
      Angle        => 1.57,
      Turns_Around => Black_Hole);

   --  create an infinite loop
   --  update the Now time with current clock
   --  wait until Now + Period time elapsed before the next
   loop
      Now := Clock;
      delay until Now + Period;
   end loop;

end Main;
