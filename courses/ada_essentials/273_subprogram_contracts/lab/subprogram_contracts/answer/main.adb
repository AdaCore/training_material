--|main_part_1_start
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with Priority_Queue; use Priority_Queue;
procedure Main is

   function Build (Priority : Priority_T;
                   Count    : Integer)
                   return Item_T;
   procedure Print (Queue : in out Queue_T);

   procedure Normal is
      Queue : Queue_T;
   begin
      Put_Line ("Normal Processing");
      for Count in 1 .. 3 loop
         for Priority in Priority_T'Range loop
            Queue.Push (Build (Priority, Count));
         end loop;
      end loop;
      Print (Queue);
   end Normal;

   procedure Overflow is
      Queue : Queue_T;
   begin
      Put_Line ("Overflow Processing");
      for Count in 1 .. 100 loop
         for Priority in Priority_Queue.Priority_T'Range loop
            Queue.Push (Build (Priority, Count));
         end loop;
      end loop;
      Print (Queue);
   exception
      when The_Err : others =>
         Put_Line ("  Failed with " & Exception_Information (The_Err));
   end Overflow;

   procedure Underflow is
      Queue : Queue_T;
      Value : Value_T;
   begin
      Put_Line ("Underflow Processing");
      Queue.Pop (Value);
      Put_Line (Value'Image);
   exception
      when The_Err : others =>
         Put_Line ("  Failed with " & Exception_Information (The_Err));
   end Underflow;

   procedure Duplicate is
      Queue : Queue_T;
   begin
      Put_Line ("Duplicate Processing");
      Queue.Push ((Priority_T'First, 123));
      Queue.Push ((Priority_T'First, 123));
      Print (Queue);
   exception
      when The_Err : others =>
         Put_Line ("  Failed with " & Exception_Information (The_Err));
   end Duplicate;
--|main_part_1_end

--|main_part_2_start
   function Build (Priority : Priority_T;
                   Count    : Integer)
                   return Item_T is
      Retval : Item_T;
      Value  : constant Integer :=
        (1 + Priority_T'Pos (Priority)) * 100 + Count;
   begin
      Retval.Priority := Priority;
      Retval.Value    := Value_T (Value);
      return Retval;
   end Build;

   procedure Print (Queue : in out Queue_T) is
      Value : Value_T;
   begin
      while not Queue.Empty loop
         Queue.Pop (Value);
         Put_Line (Value'Image);
      end loop;
   end Print;

begin

   Normal;
   Overflow;
   Underflow;
   Duplicate;

end Main;
--|main_part_2_end
