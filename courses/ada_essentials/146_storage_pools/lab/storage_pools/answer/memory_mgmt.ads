with System.Storage_Elements;
with System.Storage_Pools;

package Memory_Mgmt is

   type Storage_Pool_T is new System.Storage_Pools.Root_Storage_Pool
   with null record;

   procedure Print_Info;

   procedure Allocate
     (Pool                       : in out Storage_Pool_T;
      Storage_Address            : out System.Address;
      Size_In_Storage_Components : System.Storage_Elements.Storage_Count;
      Alignment                  : System.Storage_Elements.Storage_Count);
   procedure Deallocate
     (Pool                       : in out Storage_Pool_T;
      Storage_Address            : System.Address;
      Size_In_Storage_Components : System.Storage_Elements.Storage_Count;
      Alignment                  : System.Storage_Elements.Storage_Count);
   function Storage_Size
     (Pool : Storage_Pool_T) return System.Storage_Elements.Storage_Count;

   Storage_Pool : Storage_Pool_T;

end Memory_Mgmt;
