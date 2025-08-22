with Ada.Text_IO; use Ada.Text_IO;
with Counter;     use Counter;
with Datastore;   use Datastore;
procedure Main is
  Counters : array (Register_T) of Counter_T;

  function Get_Line return String is
    S : String (1 .. 100);
    L : Integer;
  begin
    Get_Line (S, L);
    return S (1 .. L);
  end Get_Line;

  function Get (Prompt : String) return Integer is
  begin
    Put ("  " & Prompt & ">");
    return Integer'Value (Get_Line);
  end Get;

  procedure Print is
  begin
    for Register in Register_T loop
         Put_Line
           (Register_T'Image (Register) & " =>" &
            Integer'Image (Datastore.Read (Register)));
    end loop;
  end Print;

begin
  for Register in Register_T loop
    Put_Line ("Register " & Register_T'Image (Register));
    declare
      V : constant Integer := Get ("Initial value");
      I : constant Integer := Get ("Increment");
      D : constant Integer := Get ("Delay in tenths");
    begin
      Counters (Register).Initialize (Register   => Register,
                                      Value      => V,
                                      Increment  => I,
                                      Delay_Time => Duration (D) / 10.0);
    end;
  end loop;

  loop
    Put_Line ("Enter Q to quit, any other value to print registers");
    declare
      Str : constant String := Get_Line;
    begin
      exit when Str'Length > 0
        and then (Str (Str'First) = 'Q' or else Str (Str'First) = 'q');
      Print;
    end;
  end loop;

  for Register in Register_T loop
    abort Counters (Register);
  end loop;
end Main;
