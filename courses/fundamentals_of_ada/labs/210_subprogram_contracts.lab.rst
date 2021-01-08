--------------------------
Subprogram Contracts Lab
--------------------------

* Overview

   - Create a priority-based queue ADT

      + Higher priority items come off the queue first
      + When priorities are the same, entries should be processed in order received

* Requirements

   - Main program should verify pre-condition failure(s)

      - At least one pre-condition should raise something other than assertion error

   - Post-condition should ensure queue is correctly ordered

* Hints

   - This is basically a stack, except insertion doesn't necessarily happen at one end

--------------------------------------------------
Subprogram Contracts Lab Solution - Queue (Spec)
--------------------------------------------------

.. code:: Ada

   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   package Priority_Queue is
      Overflow : exception;
      type Priority_T is (Low, Medium, High);
      type Queue_T is tagged private;
      procedure Push (Queue    : in out Queue_T;
                      Priority :        Priority_T;
                      Value    :        String)
         with Pre => (not Full (Queue) and then Value'Length > 0) or else raise Overflow,
              Post => Valid (Queue);
      procedure Pop (Queue : in out Queue_T;
                     Value :    out Unbounded_String)
         with Pre  => not Empty (Queue),
              Post => Valid (Queue);
      function Full (Queue : Queue_T) return Boolean;
      function Empty (Queue : Queue_T) return Boolean;
      function Valid (Queue : Queue_T) return Boolean;
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
   
--------------------------------------------------
Subprogram Contracts Lab Solution - Queue (Body)
--------------------------------------------------

.. code:: Ada

   package body Priority_Queue is
      procedure Push (Queue    : in out Queue_T;
                      Priority :        Priority_T;
                      Value    :        String) is
         Last      : Size_T renames Queue.Size;
         New_Entry : Entries_T := (Priority, To_Unbounded_String (Value));
      begin
         if Queue.Size = 0 then
            Queue.Entries (Last + 1) := New_Entry;
         elsif Priority < Queue.Entries (1).Priority then
            Queue.Entries
              (2 .. Last + 1) := Queue.Entries (1 .. Last);
            Queue.Entries (1) := New_Entry;
         elsif Priority > Queue.Entries (Last).Priority then
            Queue.Entries (Last + 1) := New_Entry;
         else
            for Index in 1 .. Last loop
               if Priority <= Queue.Entries (Index).Priority then
                  Queue.Entries
                    (Index + 1 .. Last + 1) := Queue.Entries (Index .. Last);
                  Queue.Entries (Index)     := New_Entry;
                  exit;
               end if;
            end loop;
         end if;
         Last := Last + 1;
      end Push;
   
      procedure Pop (Queue : in out Queue_T;
                     Value :    out Unbounded_String) is
      begin
         Value      := Queue.Entries (Queue.Size).Value;
         Queue.Size := Queue.Size - 1;
      end Pop;
   end Priority_Queue;
   
-------------------------------------------
Subprograms Contracts Lab Solution - Main
-------------------------------------------

.. code:: Ada

   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   with Ada.Text_IO;           use Ada.Text_IO;
   with Priority_Queue;
   procedure Main is
      Queue : Priority_Queue.Queue_T;
      Value : Unbounded_String;
   begin
   
      for Count in 1 .. 3 loop
         for Priority in Priority_Queue.Priority_T'Range
         loop
            Queue.Push (Priority, Priority'Image & Count'Image);
         end loop;
      end loop;
   
      while not Queue.Empty loop
         Queue.Pop (Value);
         Put_Line (To_String (Value));
      end loop;
   
      for Count in 1 .. 4 loop
         for Priority in Priority_Queue.Priority_T'Range
         loop
            Queue.Push (Priority, Priority'Image & Count'Image);
         end loop;
      end loop;
   
   end Main;
   

