------------------------------------------------------------------------------
--                                 GNATfuzz                                 --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
--
--  From the user, an air temperature (T) is given. Before calculating the
--  speed of sound (Knots), the air temperature value must be converted to
--  Kelvin (K). The speed of sound is then converted to the corresponding
--  unit of the user provided measured air speed and the Mach value
--  calculated and returned.
--
--  *** THIS CODE CONTAINS MULTIPLE (AND INTENTIONAL) BUGS. ITS PRIMARY
--      PURPOSE IS TO DEMONSTRATE THE CAPABILITIES OF ADACORE TOOLING
--      (GNATFUZZ, GNATTEST, CODEPEER, SPARK PRO ETC).
--      IT IS NOT INTENDED FOR COMERCIAL USE ***

package Mach_Calculator is

   subtype Temperature_Type is Float range -2_000.0 .. 2_000.0;
   --  Universal temperature value type

   type Temperature_Unit_Type is (Faherheit, Celsius, Kelvin, Rankine);
   --  The unit the temperature value was measured in

   subtype Speed_Type is Float range 0.0 .. 3_000.0;
   --  Universal speed value type

   type Speed_Unit_Type is
     (Miles_Per_Hour, Kilometers_Per_Hour, Knots, Meters_Per_Second,
      Feet_Per_Second);
   --  The unit the speed value was measured in

   type Sensor_Data_Type is record
      Air_Temperature  : Temperature_Type;
      Temperature_Unit : Temperature_Unit_Type;
      Air_Speed        : Speed_Type;
      Speed_Unit       : Speed_Unit_Type;
   end record;
   --  Representative data read from the equipment sensors

   subtype Mach_Type is Float range 0.0 .. 20.0;
   --  Range of the calculated Mach speed for the vehicle

   function Get_Mach_Value (Sensor_Data : Sensor_Data_Type) return Mach_Type;
   --  Calculates the speed of sound using the provided air temperature and
   --  returns the Mach value associated with the provided air speed

private

   --  *** Air temperature conversions ***

   function Faherheit_To_Kelvin
     (Faherheit_Value : Temperature_Type) return Temperature_Type;
   --  Given a value in Faherheit return the corresponding value in Kelvin

   function Celsius_To_Kelvin
     (Celsius_Value : Temperature_Type) return Temperature_Type;
   --  Given a value in Celsius return the corresponding value in Kelvin

   function Rankin_To_Kelvin
     (Rankin_Value : Temperature_Type) return Temperature_Type;
   --  Given a value in Rankin return the corresponding value in Kelvin

   --  *** Air speed conversions ***

   function Knots_To_Miles_Per_Hour
     (Knots_Value : Speed_Type) return Speed_Type;
   --  Given a value in Knots return the corresponding value in Miles Per Hour

   function Knots_To_Meters_Per_Second
     (Knots_Value : Speed_Type) return Speed_Type;
   --  Given a value in Knots return the corresponding value in Meters Per
   --  Second

   function Knots_To_Feet_Per_Second
     (Knots_Value : Speed_Type) return Speed_Type;
   --  Given a value in Knots return the corresponding value in Feet Per Second

   function Knots_To_Kilometers_Per_Hour
     (Knots_Value : Speed_Type) return Speed_Type;
   --  Given a value in Knots return the corresponding value in Kilometers Per
   --  Hour

end Mach_Calculator;
