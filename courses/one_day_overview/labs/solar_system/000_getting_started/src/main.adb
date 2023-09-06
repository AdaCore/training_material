-----------------------------------------------------------------------
--                              Ada Labs                             --
--                                                                   --
--                 Copyright (C) 2008-2019, AdaCore                  --
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

with Display;       use Display;
with Display.Basic; use Display.Basic;
with Ada.Real_Time; use Ada.Real_Time;
--with Last_Chance_Handler; pragma Unreferenced (Last_Chance_Handler);

procedure Main is
   Width       : constant := 240.0;
   Height      : constant := 320.0;
   Ball_Radius : constant := 20.0;

   X       : Float              := 0.0;
   Y       : Float              := 0.0;
   Speed_X : Float              := 2.0;
   Speed_Y : Float              := 4.0;
   Next    : Time;
   Period  : constant Time_Span := Milliseconds (40);

   --  reference to the application window
   Window : Window_Id;

   --  reference to the graphical canvas associated with the application window
   Canvas : Canvas_Id;

begin
   Window :=
     Create_Window
       (Width  => Integer (Width),
        Height => Integer (Height),
        Name   => "Bouncing ball");
   Canvas := Get_Canvas (Window);

   Next := Clock + Period;

   while not Is_Killed loop

      if (abs X) + Ball_Radius >= Width / 2.0 then
         Speed_X := -Speed_X;
      end if;

      if (abs Y) + Ball_Radius >= Height / 2.0 then
         Speed_Y := -Speed_Y;
      end if;
      X := X + Speed_X;
      Y := Y + Speed_Y;

      Draw_Sphere
        (Canvas   => Canvas,
         Position => (X, Y, 0.0),
         Radius   => Ball_Radius,
         Color    => Red);

      Swap_Buffers (Window);

      delay until Next;
      Next := Next + Period;

   end loop;

end Main;
