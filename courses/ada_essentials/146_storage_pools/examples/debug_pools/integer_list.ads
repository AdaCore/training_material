with Memory_Mgmt;
package Integer_List is
   type List_T is limited private;
   procedure First (List : in out List_T);
   procedure Next (List : in out List_T);
   function End_Of_List
     (List : List_T)
      return Boolean;
   function Current
     (List : List_T)
      return Integer;
   procedure Insert
     (List      : in out List_T;
      Component :        Integer);
   procedure Delete
     (List      : in out List_T;
      Component :        Integer);
   function Is_Empty
     (List : List_T)
      return Boolean;
private
   type Linked_List_T;
   type Linked_List_Ptr_T is access all Linked_List_T;
   for Linked_List_Ptr_T'Storage_Pool use Memory_Mgmt.Storage_Pool;
   type Linked_List_T is record
      Next    : Linked_List_Ptr_T;
      Content : Integer;
   end record;
   type List_T is record
      Head    : Linked_List_Ptr_T;
      Current : Linked_List_Ptr_T;
   end record;
end Integer_List;
