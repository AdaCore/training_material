with Ada.Text_IO; use Ada.Text_IO;
with List;
with Types;
with Validator;
procedure Main is

begin

   List.Add (12.34, '+', 56.78);
   List.Add (12.34, '-', 56.78);
   List.Add (12.34, '*', 56.78);
   List.Add (12.34, '/', 56.78);

   for Index in 1 .. List.Length loop
      declare
         Item : constant Types.Record_T := List.Element (Index);
      begin
         Put_Line
           (Types.Image (Item) & " " &
            Boolean'Image (Validator.Is_Valid (Item)));
      end;
   end loop;

end Main;
