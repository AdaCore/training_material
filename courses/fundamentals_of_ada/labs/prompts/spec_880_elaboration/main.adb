with Ada.Text_IO; use Ada.Text_IO;
with Datastore;
procedure Main is

begin

   for I in Datastore.Object'First .. Datastore.Object'Last loop
      -- Only print valid values
      null;
   end loop;

end Main;
