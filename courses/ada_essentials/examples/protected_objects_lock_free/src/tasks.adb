with Ada.Text_IO;
with Protected_Objects; use Protected_Objects;

package body Tasks is

   task body T1 is
   begin
      accept Start do
         Object.Set (0);
      end Start;
      loop
         accept Receive_Message do
            declare
               V : Integer := Object.Get;
            begin
                Object.Set (V + 1);
                Ada.Text_IO.Put_Line (V'Image);
            end;
         end Receive_Message;
      end loop;
   end T1;

   task body T2 is
   begin
      accept Start do
         Object.Set (0);
      end Start;
      loop
         accept Receive_Message do
            declare
               V : Integer := Object.Get;
            begin
                Object.Set (V * 2);
                Ada.Text_IO.Put_Line (V'Image);
            end;
         end Receive_Message;
      end loop;
   end T2;

end Tasks;
