with Database; use Database;
package Database_List is
   type List_T is limited private;
   procedure First (List : in out List_T);
   procedure Next (List : in out List_T);
   function End_Of_List
     (List : List_T)
      return Boolean;
   function Current
     (List : List_T)
      return Database_T;
   procedure Insert
     (List      : in out List_T;
      Component :        Database_T);
   procedure Delete
     (List      : in out List_T;
      Component :        Database_T);
   function Is_Empty
     (List : List_T)
      return Boolean;
private
   type List_T is null record;
end Database_List;
