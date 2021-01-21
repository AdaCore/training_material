with Ada.Text_IO; use Ada.Text_IO;
with Data_Type;
with Generic_List;
with Generic_List.Output;
use type Data_Type.Record_T;
procedure Main is

  package List is new Generic_List (Data_Type.Record_T, 10);
  package Output is new List.Output (Data_Type.Image);

  My_List : List.List_T;

  Element : Data_Type.Record_T;

begin

  loop
    Put ("Enter character: ");
    declare
      Str : constant String := Get_Line;
    begin
      exit when Str'Length = 0;
      Element.Field2 := Str (1);
    end;
    Put ("Enter number: ");
    declare
      Str : constant String := Get_Line;
    begin
      exit when Str'Length = 0;
      Element.Field1 := Integer'Value (Str);
    end;
    My_List.Add (Element);
  end loop;

  My_List.Sort;

  Output.Print (My_List);
  null;
end Main;
