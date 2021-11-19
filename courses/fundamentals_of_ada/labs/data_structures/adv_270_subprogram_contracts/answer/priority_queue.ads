with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Priority_Queue is
   Overflow : exception;

   type Priority_T is (Low, Medium, High);
   type Queue_T is tagged private;
   
   -- Question 1
   -- Implement these 3 predicates
   function Full (Queue : Queue_T) return Boolean;
   function Empty (Queue : Queue_T) return Boolean;
   function Valid (Queue : Queue_T) return Boolean;

   -- Question 2a
   -- Implement the priority handling for this queue

   -- Question 2b
   -- Add pre and post condition to this subprogram
   procedure Push (Queue    : in out Queue_T;
                   Priority :        Priority_T;
                   Value    :        String)
      with
        Pre => (not Full (Queue) and then Value'Length > 0) or else raise Overflow,
        Post => Valid (Queue)
      ;

   -- Question 3
   -- Add pre and post condition to this subprogram
   procedure Pop (Queue : in out Queue_T;
                  Value :    out Unbounded_String)
      with
        Pre  => not Empty (Queue),
        Post => Valid (Queue)
      ;

private

   Max_Queue_Size : constant := 10;

   type Entries_T is record
      Priority : Priority_T;
      Value    : Unbounded_String;
   end record;
   
   type Size_T is range 0 .. Max_Queue_Size;
   type Queue_Array_T is array (1 .. Size_T'Last) of Entries_T;

   type Queue_T is tagged record
      Size    : Size_T := 0;
      Entries : Queue_Array_T;
   end record;

   function Full (Queue : Queue_T) return Boolean is (Queue.Size = Size_T'Last);
   function Empty (Queue : Queue_T) return Boolean is (Queue.Size = 0);
   
   function Valid (Queue : Queue_T) return Boolean is
     (if Queue.Size <= 1 then True
      else (for all Index in 2 .. Queue.Size =>
               Queue.Entries (Index).Priority >=
               Queue.Entries (Index - 1).Priority));
end Priority_Queue;
