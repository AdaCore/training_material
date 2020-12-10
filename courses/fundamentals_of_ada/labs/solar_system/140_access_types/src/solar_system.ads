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

with Display;       use Display;
with Display.Basic; use Display.Basic;

package Solar_System is

   --  define type Bodies_Enum_T as an enumeration of Sun, Earth, Moon, Satellite ...
   type Bodies_Enum_T is
     (Sun, Earth, Moon, Satellite, Comet, Black_Hole, Asteroid_1, Asteroid_2);

   --  TODO : declare type Body_Access_T as an access type to all Body_T

   type Bodies_Array_T is private;

   --  TODO : Implement Get_Body returning an access of type Body_Access_T
   --  function Get_Body (B : Bodies_Enum_T; Bodies : access Bodies_Array_T) return Body_Access_T;

   --  TODO : Modify Init_Body with the following profile
   --      procedure Init_Body (B           : Body_Access_T;
   --                          Radius       : Float;
   --                          Color        : RGBA_T;
   --                          Distance     : Float;
   --                          Angle        : Float;
   --                          Speed        : Float;
   --                          Turns_Around : Body_Access_T;
   --                          Visible      : Boolean := True);

   procedure Init_Body
     (B            :        Bodies_Enum_T;
      Bodies       : in out Bodies_Array_T;
      Radius       :        Float;
      Color        :        RGBA_T;
      Distance     :        Float;
      Angle        :        Float;
      Speed        :        Float;
      Turns_Around :        Bodies_Enum_T;
      Visible      :        Boolean := True);

   procedure Move_All (Bodies : access Bodies_Array_T);

private
   --  define a type Body_T to store every information about a body
   --  X, Y, Distance, Speed, Angle, Radius, Color
   type Body_T is record
      X        : Float   := 0.0;
      Y        : Float   := 0.0;
      Distance : Float   := 0.0;
      Speed    : Float   := 0.0;
      Angle    : Float   := 0.0;
      Radius   : Float   := 0.0;
      Color    : RGBA_T;
      Visible  : Boolean := True;
      --  TODO : Modify Turns_Around as an access to Body_T using Body_Access_T type
      Turns_Around : Bodies_Enum_T := Sun;
   end record;

   --  define type Bodies_Array_T as an array of Body_T indexed by bodies enumeration
   type Bodies_Array_T is array (Bodies_Enum_T) of Body_T;

   --  TODO : Modify the Move procedure with the following profile
   --  procedure Move (Body_To_Move : Body_Access_T);
   procedure Move (Body_To_Move : in out Body_T; Bodies : Bodies_Array_T);

end Solar_System;
