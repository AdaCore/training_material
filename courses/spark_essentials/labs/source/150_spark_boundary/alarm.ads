with System.Storage_Elements;

package Alarm is
   type Alarm_Status is (On, Off);

   function Get_Temperature return Integer;

   function Get_Status return Alarm_Status;

   procedure Set_Status;

private

   Temperature : Integer with
     Address => System.Storage_Elements.To_Address (16#FFFF_FFF0#),
     Volatile;

   Status : Alarm_Status := Off with
     Address => System.Storage_Elements.To_Address (16#FFFF_FFF4#),
     Volatile;

end Alarm;
