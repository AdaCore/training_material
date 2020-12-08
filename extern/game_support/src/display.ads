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

package Display is

   type Color_Component_T is mod 2**8;

   type RGBA_T is record
      R : Color_Component_T;
      G : Color_Component_T;
      B : Color_Component_T;
      A : Color_Component_T;
   end record with Pack, Size => 32;

   Transparent : RGBA_T  := (R => 0, G => 0, B => 0, A => 0);
   Black : RGBA_T  := (R => 0, G => 0, B => 0, A => 255);
   White : RGBA_T  := (R => 255, G => 255, B => 255, A => 255);
   Blue : RGBA_T  := (R => 0, G => 0, B => 255, A => 255);
   Green : RGBA_T  := (R => 0, G => 255, B => 0, A => 255);
   Red : RGBA_T  := (R => 255, G => 0, B => 0, A => 255);
   Cyan : RGBA_T  := (R => 0, G => 255, B => 255, A => 255);
   Magenta : RGBA_T  := (R => 255, G => 0, B => 255, A => 255);
   Yellow : RGBA_T  := (R => 255, G => 255, B => 0, A => 255);
   Gray : RGBA_T  := (R => 128, G => 128, B => 128, A => 255);


   Display_Error : exception;

   type Button_Type is (Left, Right);

   type Mouse_Position is record
      X, Y : Float;
      Button : Button_Type;
   end record;

   No_Mouse_Position : constant Mouse_Position := (-10_000.0, -10_000.0, Right);


end Display;
