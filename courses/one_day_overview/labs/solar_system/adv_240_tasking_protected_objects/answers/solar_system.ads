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

   --  define type Bodies_Enum as an enumeration of Sun, Earth, Moon, Satellite
   type Bodies_Enum_T is
     (Sun, Earth, Moon, Satellite, Comet, Black_Hole, Asteroid_1, Asteroid_2);

   procedure Init_Body
     (B            : Bodies_Enum_T;
      Radius       : Float;
      Color        : RGBA_T;
      Distance     : Float;
      Speed        : Float;
      Turns_Around : Bodies_Enum_T;
      Angle        : Float   := 0.0;
      Tail         : Boolean := False;
      Visible      : Boolean := True);

   type Body_T is private;


   function Get_Body(B : Bodies_Enum_T) return Body_T;

private

   type Position_T is record
      X : Float := 0.0;
      Y : Float := 0.0;
   end record;

   type Tail_Length_T is new Integer range 1 .. 10;
   type Tail_T is array (Tail_Length_T) of Position_T;

   type Body_T is record
      Pos          : Position_T;
      Distance     : Float;
      Speed        : Float;
      Angle        : Float;
      Radius       : Float;
      Color        : RGBA_T;
      Visible      : Boolean := True;
      Turns_Around : Bodies_Enum_T;
      With_Tail    : Boolean := False;
      Tail         : Tail_T  := (others => (0.0, 0.0));
   end record;

   protected Dispatch_Tasks is
      procedure Get_Next_Body (B : out Bodies_Enum_T);
   private
      Current : Bodies_Enum_T := Bodies_Enum_T'First;
   end Dispatch_Tasks;

   task type T_Move_Body;

   type Task_Array_T is array (Bodies_Enum_T) of T_Move_Body;
   Tasks : Task_Array_T;

   protected type Body_P is
      function Get_Data return Body_T;
      procedure Set_Data (B : Body_T);
   private
      Data : Body_T;
   end Body_P;

   type Bodies_Array_T is array (Bodies_Enum_T) of Body_P;
   Bodies : Bodies_Array_T;

   procedure Move (Body_To_Move : in out Body_T; Turns_Around : Body_T);

end Solar_System;
