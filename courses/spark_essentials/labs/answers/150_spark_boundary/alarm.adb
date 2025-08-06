package body Alarm
  with Refined_State => (Input_State => Temperature,
                         Output_State => Status)
is
   function Get_Temperature return Integer is
      Current : Integer := Temperature;
   begin
      return Current;
   end Get_Temperature;

   function Get_Status return Alarm_Status is
   begin
      return Status;
   end Get_Status;

   procedure Set_Status is
      Current : Integer := Get_Temperature;
   begin
      if Current > 100 then
         Status := On;
      end if;
   end Set_Status;

end Alarm;
