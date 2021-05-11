with Ada.Text_IO; use Ada.Text_IO;
with Constants;
with Datastore;
procedure Main is

begin

  for I in Datastore.Object'First .. Datastore.Object'Last loop
    exit when Datastore.Object (I) = Constants.Invalid_Value;
    Put_Line
     (Integer'Image (I) & " =>" & Integer'Image (Datastore.Object (I)));
  end loop;

end Main;
