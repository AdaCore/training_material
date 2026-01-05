with Ada.Text_IO;      use Ada.Text_IO;
with GNAT.Debug_Pools; use GNAT.Debug_Pools;
with Integer_List;     use Integer_List;
with Memory_Mgmt;      use Memory_Mgmt;
procedure Main is
   List : List_T;
begin
   Insert (List, 123);
   Delete (List, 123);
   Put_Line ("High: " & High_Water_Mark (Storage_Pool)'Image);
   Put_Line ("Current: " & Current_Water_Mark (Storage_Pool)'Image);
end Main;
