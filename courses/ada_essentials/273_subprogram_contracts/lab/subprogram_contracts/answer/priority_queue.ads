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

   procedure Push
     (Queue : in out Queue_T;
      Item  :        Item_T) with
     Pre  =>
      (if Full (Queue) then raise Overflow)
      and then
      (if Contains (Queue, Item.Value) then
         raise Duplicate_Item with Item.Value'Image),
     Post =>
      Length (Queue) = Length (Queue'Old) + 1
      and then Is_Consistent (Queue'Old, Queue, Item.Value)
      and then Is_Ordered (Queue);
   procedure Pop
     (Queue : in out Queue_T;
      Value :    out Value_T) with
     Pre  => not Empty (Queue),
     Post =>
      Length (Queue) = Length (Queue'Old) - 1
      and then not (Contains (Queue, Value))
      and then Is_Consistent (Queue, Queue'Old, Value)
      and then Is_Ordered (Queue);

   function Full
     (Queue : Queue_T)
      return Boolean;
   function Empty
     (Queue : Queue_T)
      return Boolean;
   function Length
     (Queue : Queue_T)
      return Integer;
   function Is_Ordered
     (Queue : Queue_T)
      return Boolean;
   function Contains
     (Queue : Queue_T;
      Value : Value_T)
      return Boolean;
   function Is_Consistent
     (Before : Queue_T;
      After  : Queue_T;
      Value  : Value_T)
      return Boolean;
private
   Max_Queue_Size : constant := 100;
   type Size_T is range 0 .. Max_Queue_Size;
   type Queue_Array_T is array (1 .. Size_T'Last) of Item_T;
   type Queue_T is tagged record
      Size    : Size_T := 0;
      Entries : Queue_Array_T;
   end record;

   function Length
     (Queue : Queue_T)
      return Integer is (Integer (Queue.Size));

   function Full
     (Queue : Queue_T)
      return Boolean is (Queue.Size = Size_T'Last);
   function Empty
     (Queue : Queue_T)
      return Boolean is (Queue.Size = 0);

   function Is_Ordered
     (Queue : Queue_T)
      return Boolean is
     (if Queue.Size <= 1 then True
      else
        (for all Index in 2 .. Queue.Size =>
           Queue.Entries (Index).Priority >=
           Queue.Entries (Index - 1).Priority));

   function Contains
     (Queue : Queue_T;
      Value : Value_T)
      return Boolean is
     (if Empty (Queue) then False
      else
        (for some Index in 1 .. Queue.Size =>
           Queue.Entries (Index).Value = Value));

   function Is_Consistent
     (Before : Queue_T;
      After  : Queue_T;
      Value  : Value_T)
      return Boolean is
     (Contains (After, Value)
      and then
      (for all Index in 1 .. Before.Size =>
         Contains (After, Before.Entries (Index).Value)));

end Priority_Queue;
