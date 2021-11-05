with Ada.Text_IO; use Ada.Text_IO;
with Datastore;   use Datastore;
procedure Main is
   function Get
     (Prompt : String)
      return String is
   begin
      Put ("   " & Prompt & "> ");
      return Get_Line;
   end Get;

   Index : Integer;

begin
   loop
      Index := Integer'value (Get ("Enter index"));
      exit when Index not in Datastore.Index_T'range;
      -- Add a user-supplied string to the array at the specified index
   end loop;

   for I in Index_T'range loop
      -- If the object pointed to by index is not empty,
      --    Print each item in the list
      null;
   end loop;
end Main;
--Main
