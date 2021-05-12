package body Sensors is
   type Counter_T is mod 9;
   Values : constant array ( Counter_T ) of Sensor_T :=
         ( 0, 1, 2, 4, 8, 16, 32, 64, 128 );
   Counter : Counter_T := Counter_T'first;
   function Value return Sensor_T is
   begin
      Counter := Counter + 1;
      return Values(Counter);
   end Value;
end Sensors;

