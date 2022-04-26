with Interfaces;
with System.Storage_Elements;
package body Sender is

   type Speed_Message_Buffer_T is record
      Valid   : Boolean := False;
      Message : Speed_Message_T;
   end record;

   procedure Send (Message : Speed_Message_T) is
      Buffer : Speed_Message_Buffer_T with
         Volatile,
         Address => System.Storage_Elements.To_Address (16#3333_4444#);
   begin
      Buffer :=
        (True,
         Message);
      delay 0.1;
      Buffer.Valid := False;
   end Send;

end Sender;
