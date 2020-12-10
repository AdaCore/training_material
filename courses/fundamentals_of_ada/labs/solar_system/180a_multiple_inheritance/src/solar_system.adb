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

with Libm_Single; use Libm_Single;

package body Solar_System is

   function Get_Body (B : Bodies_Enum_T; Bodies : access Bodies_Array_T) return Body_Access_T is

   begin
      return Bodies (B)'Access;
   end Get_Body;

   procedure Init_Body (B            : Body_Access_T;
                        Radius       : Float;
                        Color        : RGBA_T;
                        Distance     : Float;
                        Angle        : Float;
                        Speed        : Float;
                        Turns_Around : Body_Access_T;
                        Visible      : Boolean := True) is
   begin

      B.all := (Distance     => Distance,
                Speed        => Speed,
                Angle        => Angle,
                Turns_Around => Turns_Around,
                Visible      => Visible,
                Radius       => Radius,
                Color        => Color,
                others       => <>);


   end Init_Body;


   -- implement a function to compute the X coordinate
   -- x of the reference + distance * cos(angle)
   function Compute_X(Body_To_Move : Body_T; Turns_Around : Body_T) return Float;

   -- implement a function to compute the Y coordinate
   -- y of the reference + distance * sin(angle)
   function Compute_Y(Body_To_Move : Body_T; Turns_Around : Body_T) return Float;

   function Compute_X(Body_To_Move : Body_T; Turns_Around : Body_T) return Float is
   begin
      return Turns_Around.X + Body_To_Move.Distance * Cos (Body_To_Move.Angle);
   end Compute_X;

   function Compute_Y(Body_To_Move : Body_T; Turns_Around : Body_T) return Float is
   begin
      return Turns_Around.Y + Body_To_Move.Distance * Sin (Body_To_Move.Angle);
   end Compute_Y;

   procedure Move (Body_To_Move : Body_Access_T) is
   begin

      Body_To_Move.X :=  Compute_X(Body_To_Move.all, Body_To_Move.Turns_Around.all);

      Body_To_Move.Y := Compute_Y(Body_To_Move.all, Body_To_Move.Turns_Around.all);

     Body_To_Move.Angle := Body_To_Move.Angle + Body_To_Move.Speed;
   end Move;

   procedure Move_All (Bodies : access Bodies_Array_T) is
   begin

      -- loop over all bodies and call Move procedure
      for B in Bodies_Enum_T loop

         -- call the move procedure for each body
         Move (Get_Body (B, Bodies));
      end loop;

   end Move_All;

end Solar_System;
