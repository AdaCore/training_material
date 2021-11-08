with Ada.Text_IO; use Ada.Text_IO;

package body Tasks is

   task body T is
   begin
      loop
         accept Start do
            Put_Line ("Start");
         end Start;
         accept Receive_Message (V : String) do
            Put_Line ("Receive " & V);
         end Receive_Message;
      end loop;
   end T;

end Tasks;
