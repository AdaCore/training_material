with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type A is array (Positive range <>) of Integer;
   O : A := [for I in 1 .. 10
              => (if I * I > 1 and I * I < 20 then I else 0)];

   procedure Put_Line (O : A) is
   begin
      Put ("[");
      for J in O'Range loop
          Put (Integer'Image (O (J)));
          if J /= O'Last then
              Put (", ");
          end if;
      end loop;
      Put_Line ("]");
  end Put_Line;

  I : Integer := O'Reduce ("+", 0);
  begin

    Put_Line (O);
    Put_Line (I'Image);
    Put_Line (Integer'Image ([for E of O when E /= 0 => E]'Reduce ("*", 1)));

end Main;
