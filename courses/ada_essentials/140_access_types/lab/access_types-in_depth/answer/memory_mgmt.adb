package body Memory_Mgmt is
   procedure Print_Info is
   begin
      GNAT.Debug_Pools.Print_Info_Stdout (Storage_Pool);
   end Print_Info;
end Memory_Mgmt;
