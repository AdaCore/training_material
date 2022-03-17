--Types
package Types is
   pragma Elaborate_Body;

   type Miles_T is digits 6;
   -- Create types for at least two distance measurements (feet, meters, etc)

   type Hours_T is digits 6;
   -- Create types for at least two time measurements (seconds, minutes, etc)

   -- Create "/" operator functions to divide distance by time to return MPH_T

   -- Create helper functions to convert distance to miles and time to hours
end Types;
