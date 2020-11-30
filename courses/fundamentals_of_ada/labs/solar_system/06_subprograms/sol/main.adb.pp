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

with Libm_Single;   use Libm_Single;
with Ada.Real_Time; use Ada.Real_Time;

with Display;       use Display;
with Display.Basic; use Display.Basic;

procedure Main is

--  define type Bodies_Enum_T as an enumeration of Sun, Earth, Moon, Satellite
   type Bodies_Enum_T is (Sun, Earth, Moon, Satellite, Comet);

      --  define a type Body_T to store every information about a body
      --   X, Y, Distance, Speed, Angle, Color, Radius

   type Body_T is record
      X            : Float := 0.0;
      Y            : Float := 0.0;
      Distance     : Float;
      Speed        : Float;
      Angle        : Float;
      Color        : RGBA_T;
      Radius       : Float;
      Turns_Around : Bodies_Enum_T;
   end record;

   --  define type Bodies_Array as an array of Body_Type indexed by bodies enumeration

   type Bodies_Array_T is array (Bodies_Enum_T) of Body_T;

   --  declare variable Bodies which is an array of Body_Type
   Bodies : Bodies_Array_T;

   --  declare a variable Next of type Time to store the Next step time
   Next : Time;

   --  declare a constant Period of 40 milliseconds of type Time_Span defining the loop period
   Period : constant Time_Span := Milliseconds (40);

   --  reference to the application window
   Window : Window_ID;

   --  reference to the graphical canvas associated with the application window
   Canvas : Canvas_ID;

   --  implement a function to compute the X coordinate
   --  x of the reference + distance * cos(angle)
   function Compute_X
     (Body_To_Move : Body_T;
      Turns_Around : Body_T) return Float;

   function Compute_X
     (Body_To_Move : Body_T;
      Turns_Around : Body_T) return Float
   is
   begin
      return Turns_Around.X + Body_To_Move.Distance * Cos (Body_To_Move.Angle);
   end Compute_X;

   --  implement a function to compute the Y coordinate
   --  y of the reference + distance * sin(angle)
   function Compute_Y
     (Body_To_Move : Body_T;
      Turns_Around : Body_T) return Float;

   function Compute_Y
     (Body_To_Move : Body_T;
      Turns_Around : Body_T) return Float
   is
   begin
      return Turns_Around.Y + Body_To_Move.Distance * Sin (Body_To_Move.Angle);
   end Compute_Y;

   procedure Move (Body_To_Move : in out Body_T; Bodies : Bodies_Array_T);

   procedure Move (Body_To_Move : in out Body_T; Bodies : Bodies_Array_T) is
   begin

      Body_To_Move.X :=
        Compute_X (Body_To_Move, Bodies (Body_To_Move.Turns_Around));

      Body_To_Move.Y :=
        Compute_Y (Body_To_Move, Bodies (Body_To_Move.Turns_Around));

      Body_To_Move.Angle := Body_To_Move.Angle + Body_To_Move.Speed;

   end Move;

   procedure Draw_Body (Object : Body_T; Canvas : Canvas_ID) is
   begin
      Draw_Sphere
        (Canvas   => Canvas,
         Position => (Object.X, Object.Y, 0.0),
         Radius   => Object.Radius,
         Color    => Object.Color);
   end Draw_Body;

begin

   -- Create the main window
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
         Speed        => 0.0,
         Radius       => 20.0,
         X            => 0.0,
         Y            => 0.0,
         Angle        => 0.0,
         Color        => Yellow,
         Turns_Around => Sun),
      Earth =>
        (Distance     => 50.0,
         Speed        => 0.02,
         Radius       => 5.0,
         X            => 0.0,
         Y            => 0.0,
         Angle        => 0.0,
         Color        => Blue,
         Turns_Around => Sun),
      Moon =>
        (Distance     => 15.0,
         Speed        => 0.04,
         Radius       => 2.0,
         X            => 0.0,
         Y            => 0.0,
         Angle        => 0.0,
         Color        => White,
         Turns_Around => Earth),
      Satellite =>
        (Distance     => 8.0,
         Speed        => 0.1,
         Radius       => 1.0,
         X            => 0.0,
         Y            => 0.0,
         Angle        => 0.0,
         Color        => Red,
         Turns_Around => Earth),
      Comet =>
        (Distance     => 80.0,
         Angle        => 0.0,
         Speed        => 0.05,
         Radius       => 1.0,
         X            => 0.0,
         Y            => 0.0,
         Color        => Yellow,
         Turns_Around => Sun));

--  initialize the Next step time begin the current time (Clock) + the period
   Next := Clock + Period;

      --create an infinite loop
      --  update the Now time with current clock
      --  mov each body updating X and Y in Bodies array
   --    the position of an object around (0,0) at distance d with an angle a
      --    is (d*cos(a), d*sin(a))
   --  update angle parameter of each body adding speed to the previous angle
   --  update the screen using procedure Display.Basic.Swap_Buffers
   --  wait until Now + Period time elapsed before the next

   while not Is_Killed loop

      for B in Bodies_Enum_T loop

         --  call the move procedure for each body
         Move (Bodies (B), Bodies);
         Draw_Body (Bodies (B), Canvas);

      end loop;

      Display.Basic.Swap_Buffers (Window);

      delay until Next;
      Next := Next + Period;
   end loop;

end Main;
