with Ada.Containers.Generic_Sort;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with City_Trivia;
procedure Main is

  Trivia : City_Trivia.Strings_Vector_T;
  Keys   : City_Trivia.Strings_Vector_T;

  function Get
   (Prompt : String)
    return String is
  begin
    Put (Prompt & "> ");
    return Get_Line;
  end Get;

begin

  Outer_Loop :
  loop
    declare
      City : constant String := Get ("City name");
    begin
      exit Outer_Loop when City'Length = 0;
      Inner_Loop :
      loop
        declare
          Info : constant String := Get ("  Trivia");
        begin
          exit Inner_Loop when Info'Length = 0;
          City_Trivia.Add_Trivia
           (City        => City,
            Information => Info);
        end;
      end loop Inner_Loop;
    end;
  end loop Outer_Loop;

  Keys := City_Trivia.Get_Keys;
  City_Trivia.Sort.Sort (Keys);
  for Key of Keys loop
    Trivia := City_Trivia.Get_Trivia (To_String (Key));
    Put_Line (To_String (Key));
    for Info of Trivia loop
      Put_Line ("   " & To_String (Info));
    end loop;
  end loop;

end Main;
