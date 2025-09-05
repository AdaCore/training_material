with Ada.Text_IO; use Ada.Text_IO;
with Messages;
with Messages.Modify;

procedure Main is
   Message : Messages.Message_T;
   procedure Print is
   begin
      Put_Line ("Kind => " & Messages.Kind (Message)'Image);
      Put_Line ("Request => " & Messages.Request (Message)'Image);
      Put_Line ("Status => " & Messages.Status (Message)'Image);
      New_Line;
   end Print;
begin
   Message :=
     Messages.Create
       (Kind => Messages.Command, Request => 12.34, Status => 56);
   Print;
   Messages.Modify.Request (Message => Message, New_Value => 98.76);
   Print;
end Main;
