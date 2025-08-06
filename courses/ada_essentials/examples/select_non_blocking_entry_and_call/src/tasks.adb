with Ada.Text_IO; use Ada.Text_IO;

package body Tasks is

   task body T is
   begin
      accept Start do
         Put_Line ("T: Start");
      end Start;

      --$ begin cut
      select
         accept Receive_Message (V : String) do
            Put_Line ("T: Receive " & V);
         end Receive_Message;
      else
         Put_Line ("T: Nothing received");
      end select;
      --$ end cut

      accept Stop do
         Put_Line ("T: Stop");
      end Stop;
   end T;

end Tasks;
