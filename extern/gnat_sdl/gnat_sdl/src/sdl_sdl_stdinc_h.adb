------------------------------------------------------------------------------
--          Copyright (C) 1995-2014, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with the --
-- GNAT Pro compiler, including any required library run-time units,  using --
-- any licensing terms  of your choosing.  See the AdaCore Software License --
-- for full details.                                                        --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------
pragma Style_Checks (Off);
pragma Warnings (Off);

with System.OS_Constants; use System.OS_Constants;

package body SDL_SDL_stdinc_h is
   function SDL_putenv (variable : Interfaces.C.Strings.chars_ptr) return int is
   begin
      if Target_OS = Windows then
         declare
            function Internal (variable : Interfaces.C.Strings.chars_ptr) return int;
            pragma Import (C, Internal, "SDL_putenv");
         begin
            return Internal(variable);
         end;
      else
         declare
            function Internal (variable : Interfaces.C.Strings.chars_ptr) return int;
            pragma Import (C, Internal, "putenv");
         begin
            return Internal(variable);
         end;
      end if;
   end SDL_putenv;
end;
