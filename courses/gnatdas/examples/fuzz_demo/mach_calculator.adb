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

with Ada.Numerics.Generic_Elementary_Functions;

package body Mach_Calculator is

   --------------------
   -- Get_Mach_Value --
   --------------------

   function Get_Mach_Value (Sensor_Data : Sensor_Data_Type) return Mach_Type is
      Air_Temp_In_Kelvin : Temperature_Type;
   begin

      --  Convert the air temperature to Kelvin

      case Sensor_Data.Temperature_Unit is
         when Faherheit =>
            Air_Temp_In_Kelvin :=
              Faherheit_To_Kelvin (Sensor_Data.Air_Temperature);
         when Celsius =>
            Air_Temp_In_Kelvin :=
              Celsius_To_Kelvin (Sensor_Data.Air_Temperature);
         when Rankine =>
            Air_Temp_In_Kelvin :=
              Rankin_To_Kelvin (Sensor_Data.Air_Temperature);
         when Kelvin =>
            Air_Temp_In_Kelvin := Sensor_Data.Air_Temperature;
      end case;

      --  Calculate the speed of sound

      declare

         package Mach_Maths is new Ada.Numerics.Generic_Elementary_Functions
           (Float_Type => Temperature_Type);
         use Mach_Maths;

         Kelvin_To_Celsius_Ratio : constant Temperature_Type := 273.15;
         Base_Speed_Of_Sound     : constant Speed_Type       := 643.855;
         Speed_Of_Sound_Knots    : constant Speed_Type       :=
           Base_Speed_Of_Sound *
           ((Air_Temp_In_Kelvin / Kelvin_To_Celsius_Ratio)**0.5);

         Speed_Of_Sound_In_Relative_Sensor_Unit : Speed_Type;
      begin

         --  Convert the Speed of Sound at this air pressure to the air speed
         --  unit taken from the sensor

         case Sensor_Data.Speed_Unit is
            when Miles_Per_Hour =>
               Speed_Of_Sound_In_Relative_Sensor_Unit :=
                 Knots_To_Miles_Per_Hour (Speed_Of_Sound_Knots);
            when Kilometers_Per_Hour =>
               Speed_Of_Sound_In_Relative_Sensor_Unit :=
                 Knots_To_Kilometers_Per_Hour (Speed_Of_Sound_Knots);
            when Meters_Per_Second =>
               Speed_Of_Sound_In_Relative_Sensor_Unit :=
                 Knots_To_Meters_Per_Second (Speed_Of_Sound_Knots);
            when Feet_Per_Second =>
               Speed_Of_Sound_In_Relative_Sensor_Unit :=
                 Knots_To_Feet_Per_Second (Speed_Of_Sound_Knots);
            when Knots =>
               Speed_Of_Sound_In_Relative_Sensor_Unit := Sensor_Data.Air_Speed;
         end case;

         --  Calculate the Mach value
         return
           Mach_Type
             (Sensor_Data.Air_Speed / Speed_Of_Sound_In_Relative_Sensor_Unit);
      end;
   end Get_Mach_Value;

   -------------------------
   -- Faherheit_To_Kelvin --
   -------------------------

   function Faherheit_To_Kelvin
     (Faherheit_Value : Temperature_Type) return Temperature_Type
   is
   begin
      return (0.555_6 * (Faherheit_Value - 32.0)) + 273.16;
   end Faherheit_To_Kelvin;

   -----------------------
   -- Celsius_To_Kelvin --
   -----------------------

   function Celsius_To_Kelvin
     (Celsius_Value : Temperature_Type) return Temperature_Type
   is
   begin
      return Celsius_Value + 273.16;
   end Celsius_To_Kelvin;

   ----------------------
   -- Rankin_To_Kelvin --
   ----------------------

   function Rankin_To_Kelvin
     (Rankin_Value : Temperature_Type) return Temperature_Type
   is
   begin
      return (0.555_56 * ((Rankin_Value - 459.69) - 32.0)) + 273.16;
   end Rankin_To_Kelvin;

   -----------------------------
   -- Knots_To_Miles_Per_Hour --
   -----------------------------

   function Knots_To_Miles_Per_Hour
     (Knots_Value : Speed_Type) return Speed_Type
   is
   begin
      return 1.150_779_4 * Knots_Value;
   end Knots_To_Miles_Per_Hour;

   ----------------------------------------
   -- Convert_Knots_To_Meters_Per_Second --
   ----------------------------------------

   function Knots_To_Meters_Per_Second
     (Knots_Value : Speed_Type) return Speed_Type
   is
   begin
      return 0.514_444_4 * Knots_Value;
   end Knots_To_Meters_Per_Second;

   --------------------------------------
   -- Convert_Knots_To_Feet_Per_Second --
   --------------------------------------

   function Knots_To_Feet_Per_Second
     (Knots_Value : Speed_Type) return Speed_Type
   is
   begin
      return 1.687_809_9 * Knots_Value;
   end Knots_To_Feet_Per_Second;

   ------------------------------------------
   -- Convert_Knots_To_Kilometers_Per_Hour --
   ------------------------------------------

   function Knots_To_Kilometers_Per_Hour
     (Knots_Value : Speed_Type) return Speed_Type
   is
   begin
      return 1.852 * Knots_Value;
   end Knots_To_Kilometers_Per_Hour;

end Mach_Calculator;
