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

with System;

package body STM32F4.Device_Id is

   ID_Address : constant System.Address := System'To_Address (16#1FFF_7A10#);

   ---------------
   -- Unique_Id --
   ---------------

   function Unique_Id return Device_Id_Image is
      Result : Device_Id_Image;
      for Result'Address use ID_Address;
      pragma Import (Ada, Result);
   begin
      return Result;
   end Unique_Id;

   ---------------
   -- Unique_Id --
   ---------------

   function Unique_Id return Device_Id_Tuple is
      Result : Device_Id_Tuple;
      for Result'Address use ID_Address;
   begin
      return Result;
   end Unique_Id;

end STM32F4.Device_Id;
