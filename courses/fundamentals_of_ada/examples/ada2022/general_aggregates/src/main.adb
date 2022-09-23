with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type A is array (Positive range <>) of Integer;

   O : A := [];
   O2 : A := [1, 2];
   O3 : A := [for I in 1 .. 10
              => (if I * I > 1 and I * I < 20 then I else 0)];
   O4 : A := (for I of O3 when I /= 0 => I);

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

  begin

    Put_Line (O);
    Put_Line (O2);
    Put_Line (O3);
    Put_Line (O4);

end Main;
