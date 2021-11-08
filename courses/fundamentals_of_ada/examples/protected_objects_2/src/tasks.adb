with Protected_Objects; use Protected_Objects;

package body Tasks is

   task body T is
      My_Id : Character := ' ';
   begin
      accept Start (Id : Character; Initial_1, Initial_2 : Integer) do
         My_Id := Id;
         O1.Set (My_Id, Initial_1);
         O2.Set (My_Id, Initial_2);
      end Start;

      loop
         accept Receive_Message (Delta_1, Delta_2 : Integer) do
            declare
               New_1 : constant Integer := O1.Get + Delta_1;
               New_2 : constant Integer := O2.Get + Delta_2;
            begin
               O1.Set (My_Id, New_1);
               O2.Set (My_Id, New_2);
            end;
         end Receive_Message;
      end loop;
   end T;

end Tasks;
