with Sensor;
package Simple is
   procedure Read
     (Which  :        Sensor.Sensor_T;
      Value  : in out Integer;
      Status :    out Boolean);
   procedure Write
     (Which  :     Sensor.Sensor_T;
      Value  :     Integer;
      Status : out Boolean);
end Simple;
