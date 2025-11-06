with Ada.Text_IO; use Ada.Text_IO;
with Monitor;     use Monitor;
with Datastore;   use Datastore;

procedure Main is
   Counters : array (Register_T) of Monitor_T;

   procedure Print is
   begin
      for Register in Register_T loop
         Put_Line
           (Register_T'Image(Register)
            & " =>"
            & Integer'Image (Datastore.Read (Register)));
      end loop;
   end Print;

begin
   for Register in Register_T loop
      Put_Line ("Initialize Register " & Register_T'Image(Register));
      declare
         Factor     : constant Integer := 1 + Register_T'Pos (Register);
         Value      : constant Integer := Factor * 10 ** Factor;
         Delay_Time : constant Integer := Factor * 3;
      begin
         Counters (Register).Initialize
           (Register   => Register,
            Value      => Value,
            Increment  => 1,
            Delay_Time => Duration (Delay_Time) / 10.0);
      end;
   end loop;

   for Count in 1 .. 20 loop
      Print;
      delay 0.2;
   end loop;

   for Register in Register_T loop
      abort Counters (Register);
   end loop;
end Main;

--Main
