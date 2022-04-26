with Ada.Text_IO;
package body Sender is

   use Ada.Calendar;

   procedure Send (Message : Speed_Message_T) is
   begin
      Ada.Text_Io.Put_Line
        ("Sender.Send: Time =>" & Seconds (Message.Time)'image &
         " | Value =>" & Message.Value'image);
   end Send;

end Sender;
