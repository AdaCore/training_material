with Protected_Objects; use Protected_Objects;
package body Tasks is

   task body T1 is
   begin
      accept Start do
         Object.Set ("T1 Start", 0);
      end Start;
      loop
         accept Receive_Message do
            Object.Set ("T1 Receive", Object.Get ("T1 Receive") + 1);
         end Receive_Message;
      end loop;
   end T1;

   task body T2 is
   begin
      accept Start do
         Object.Set ("T2 Start", 0);
      end Start;
      loop
         accept Receive_Message do
            Object.Set ("T2 Receive", Object.Get ("T2 Receive") + 1);
         end Receive_Message;
      end loop;
   end T2;

end Tasks;
