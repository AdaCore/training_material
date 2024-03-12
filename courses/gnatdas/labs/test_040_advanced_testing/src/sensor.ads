package Sensor is
   type Sensor_T is (Speed, Heading, Altitude);
   function Read
     (Which : Sensor_T)
      return Integer;
   procedure Write
     (Which  :     Sensor_T;
      Value  :     Integer;
      Status : out Boolean);
end Sensor;
