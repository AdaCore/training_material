with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
package body Sensors is
   Sensor : Sensor_T;
   for Sensor use at To_Address(16#DEAD_BEEF#);
   function Value return Sensor_T is ( Sensor );
end Sensors;

