with Calendar;

procedure Main is
   Relative : Duration := 1.0;
   Absolute : Calendar.Time := Calendar.Time_Of (2030, 10, 01);
begin
   delay Relative;
   delay until Absolute;
end Main;
