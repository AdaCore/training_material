------------------------------------------------------------------------------
--                                                                          --
--                Hardware Abstraction Layer for STM32 Targets              --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with STM32F429_Discovery;  use STM32F429_Discovery;
with Ada.Real_Time;        use Ada.Real_Time;
with Fonts;
with Screen_Interface;
with STM32F4.LCD;
with Interfaces.C;
with System.Address_To_Access_Conversions;
with Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

package body Last_Chance_Handler is

   function Null_Terminated_To_String (Msg : System.Address) return String is
      package Addr_2_Char is new System.Address_To_Access_Conversions (Character);
      Max_Len : constant := 255;
      S       : String (1 .. Max_Len) := (others => 'x');
      Len     : Integer := 0;
   begin
      for I in 1 .. Max_Len loop
         declare
            C : Character;
         begin
            C :=  Character (Addr_2_Char.To_Pointer (System.Storage_Elements.To_Address (System.Storage_Elements.To_Integer (Msg) + Integer_Address (I-1))).all);
            if Character'Pos(C) /= 0 then --C >= 'a' and C <= 'z' then
               S (I) := C;
            else
               exit;
            end if;
         end;
         Len := Len + 1;
      end loop;
      declare
         R : String (1 .. Len) := S (1 .. Len);
      begin
         return R;
      end;
   end Null_Terminated_To_String;

   function Exception_To_String (Msg : System.Address; Line : Integer) return String is

   begin
      return "Err:" & Null_Terminated_To_String (Msg) & ":" & Integer'Image (Line);

   end Exception_To_String;

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is

   begin
      Screen_Interface.Fill_Screen (Col   => Screen_Interface.Black);
      Fonts.Draw_String
        (0, 0,
         Exception_To_String (Msg, Line),
         Fonts.Font12x12,
         Screen_Interface.Red,
         Screen_Interface.Black, True);

      STM32F4.LCD.Flip_Buffers;


      Initialize_LEDs;

      --  TODO: write the message and line number to the display



      Off (Green);

      --  No-return procedure...
      loop
         On (Red);
         delay until Clock + Milliseconds (500);
         Off (Red);
         delay until Clock + Milliseconds (500);
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
