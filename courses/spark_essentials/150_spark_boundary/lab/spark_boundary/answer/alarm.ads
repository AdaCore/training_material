with System.Storage_Elements;

package Alarm
  with Abstract_State =>
    ((Input_State with External => Async_Writers),
     (Output_State with External => (Async_Readers, Effective_Writes)))
is
   type Alarm_Status is (On, Off);

   function Get_Temperature return Integer
     with Volatile_Function;

   function Get_Status return Alarm_Status;

   procedure Set_Status;

private
   Temperature : Integer with
     Address => System.Storage_Elements.To_Address (16#FFFF_FFF0#),
     Volatile,
     Async_Writers,
     Part_Of => Input_State,
     Warnings => Off;

   Status : Alarm_Status := Off with
     Address => System.Storage_Elements.To_Address (16#FFFF_FFF4#),
     Volatile,
     Async_Readers,
     Effective_Writes,
     Part_Of => Output_State,
     Warnings => Off;

end Alarm;
