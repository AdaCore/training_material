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

with Solar_System;
with Display;               use Display;
with Display.Basic;         use Display.Basic;
with Solar_System.Graphics;

procedure Main is
   type Bodies_Enum_1_T is (Sun1, Earth);
   type Bodies_Enum_2_T is (Sun2, Jupiter);

   package My_Solar_System1 is new Solar_System (Bodies_Enum_1_T);
   package My_Sol_Sys_Graphics1 is new My_Solar_System1.Graphics;
   use My_Solar_System1;
   use My_Sol_Sys_Graphics1;

   package My_Solar_System2 is new Solar_System (Bodies_Enum_2_T);
   package My_Sol_Sys_Graphics2 is new My_Solar_System2.Graphics;
   use My_Solar_System2;
   use My_Sol_Sys_Graphics2;


   --  declare variable Bodies which is an array of Body_T
   Bodies1 : My_Solar_System1.Bodies_Array_T;
   Bodies2 : My_Solar_System2.Bodies_Array_T;

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

   --  initialize Bodies using Init_Body procedure
   Init_Body
     (B            => Sun1,
      Bodies       => Bodies1,
      Radius       => 20.0,
      Color        => Yellow,
      Distance     => 0.0,
      Angle        => 0.0,
      Speed        => 0.0,
      Turns_Around => Sun1);

   Init_Body
     (B            => Earth,
      Bodies       => Bodies1,
      Radius       => 5.0,
      Color        => Blue,
      Distance     => 50.0,
      Angle        => 0.0,
      Speed        => 0.02,
      Turns_Around => Sun1);

    Init_Body
     (B            => Sun2,
      Bodies       => Bodies2,
      Radius       => 20.0,
      Color        => Yellow,
      Distance     => 0.0,
      Angle        => 0.0,
      Speed        => 0.0,
      Turns_Around => Sun2);

   Init_Body
     (B            => Jupiter,
      Bodies       => Bodies2,
      Radius       => 10.0,
      Color        => (255,150,0, 255),
      Distance     => 80.0,
      Angle        => 180.0,
      Speed        => 0.01,
      Turns_Around => Sun2);

   Set_Center(Bodies1, -30.0, -30.0);
   Set_Center(Bodies2, 50.0, 50.0);


   --  initialize the Next step time begin the current time (Clock) + the period
   Next := Clock + Period;

   --  create an infinite loop
   --  call Move_All procedure
   --  call Draw_All procedure
   --  call Swap_Buffers to update the screen
   --  wait until Next time
   --  update the Next time
   while not Is_Killed loop

      Move_All (Bodies1);
      Move_All (Bodies2);

      Draw_All (Bodies1, Canvas);
      Draw_All (Bodies2, Canvas);

      Display.Basic.Swap_Buffers (Window);

      delay until Next;
      Next := Next + Period;
   end loop;

end Main;
