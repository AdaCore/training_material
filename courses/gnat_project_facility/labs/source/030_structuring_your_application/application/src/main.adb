with Odometer_Task;
procedure Main is
begin
   Odometer_Task.Monitor.Initialize;
   delay 10.0;
   Odometer_Task.Monitor.Finalize;
end Main;
