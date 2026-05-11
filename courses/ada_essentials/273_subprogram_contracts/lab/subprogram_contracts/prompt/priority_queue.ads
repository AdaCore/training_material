package Priority_Queue is
   Overflow       : exception;
   Duplicate_Item : exception;

   type Priority_T is (Low, Medium, High);
   type Value_T is new Integer;
   type Item_T is record
      Priority : Priority_T;
      Value    : Value_T;
   end record;

   type Queue_T is tagged private;

   procedure Push (Queue : in out Queue_T;
                   Item  :        Item_T);
   procedure Pop (Queue : in out Queue_T;
                  Value :    out Value_T);

   function Full (Queue : Queue_T) return Boolean;
   function Empty (Queue : Queue_T) return Boolean;
   function Length (Queue : Queue_T) return Integer;
   function Is_Ordered (Queue : Queue_T) return Boolean;

private
   Max_Queue_Size : constant := 100;
   type Size_T is range 0 .. Max_Queue_Size;
   type Queue_Array_T is array (1 .. Size_T'Last) of Item_T;
   type Queue_T is tagged record
      Size    : Size_T := 0;
      Entries : Queue_Array_T;
   end record;

   --  Implement these expression functions correctly
   function Length (Queue : Queue_T) return Integer is (0);
   function Full (Queue : Queue_T) return Boolean is (True);
   function Empty (Queue : Queue_T) return Boolean is (True);
   function Is_Ordered (Queue : Queue_T) return Boolean is (True);

end Priority_Queue;
