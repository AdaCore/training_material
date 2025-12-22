with Database; use Database;
package Database_List is
   type List_T is limited private;
   procedure First (List : in out List_T);
   procedure Next (List : in out List_T);
   function End_Of_List (List : List_T) return Boolean;
   function Current (List : List_T) return Database_T;
   procedure Insert (List      : in out List_T;
                     Component :        Database_T);
   procedure Delete (List      : in out List_T;
                     Component :        Database_T);
   function Is_Empty (List : List_T) return Boolean;
private
   type Linked_List_T;
   type Linked_List_Ptr_T is access all Linked_List_T;
   type Linked_List_T is record
      Next    : Linked_List_Ptr_T;
      Content : Database_T;
   end record;
   type List_T is record
      Head    : Linked_List_Ptr_T;
      Current : Linked_List_Ptr_T;
   end record;
end Database_List;
