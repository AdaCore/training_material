package body Database_List is

   function Is_Empty
     (List : List_T)
      return Boolean is
   begin
      return True;
   end Is_Empty;

   procedure First (List : in out List_T) is
   begin
      null;
   end First;

   procedure Next (List : in out List_T) is
   begin
      null;
   end Next;

   function End_Of_List
     (List : List_T)
      return Boolean is
   begin
      return True;
   end End_Of_List;

   function Current
     (List : List_T)
      return Database_T is
      D : Database_T;
   begin
      return D;
   end Current;

   procedure Insert
     (List      : in out List_T;
      Component :        Database_T) is
   begin
      null;
   end Insert;

   procedure Delete
     (List      : in out List_T;
      Component :        Database_T) is
   begin
      null;
   end Delete;

end Database_List;
