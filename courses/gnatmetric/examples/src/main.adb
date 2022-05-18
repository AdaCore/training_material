with Example; use Example;
with Strings; use Strings;
procedure Main is
   S1 : constant String_T := From_String ("Hello ");
   S2 : constant String_T := From_String ("World");
begin
   Example.Example (S1, S2);
end Main;
