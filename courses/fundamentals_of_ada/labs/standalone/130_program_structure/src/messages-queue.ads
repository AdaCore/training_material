
package Messages.Queue is

   function Empty return Boolean;
   function Full return Boolean;

   procedure Push (Message : Message_T);
   procedure Pop
     (Message : out Message_T;
      Valid   : out Boolean);

private

   The_Queue : array (1 .. 10) of Message_T;
   Top       : Integer := 0;

   function Empty return Boolean is (Top = 0);
   function Full return Boolean is (Top = The_Queue'Last);

end Messages.Queue;
