package body Alarm is

   function Get_Temperature return Integer is
   begin
      return Temperature;
   end Get_Temperature;

   function Get_Status return Alarm_Status is
   begin
      return Status;
   end Get_Status;

   procedure Set_Status is
   begin
      if Get_Temperature > 100 then
         Status := On;
      end if;
   end Set_Status;

end Alarm;
