.. code:: ada

   package Bounded_Stack is
      Max_Capacity : constant := 100;
      type Stack_T is private;
      procedure Push (This : in out Stack_T; Item : Integer);
      procedure Pop (This : in out Stack_T; Item : out Integer);
      function Is_Empty (This : Stack_T) return Boolean;
   private
      type Vector_T is array (1 .. Max_Capacity) of Integer;
      type Stack_T is record
         Vector : Vector_T;
         Top  : Integer range 0 .. Max_Capacity := 0;
      end record;
   end Bounded_Stack;

   package body Bounded_Stack is
      procedure Push (This : in out Stack_T; Item : Integer) is
      begin
         This.Top             := This.Top + 1;
         This.Vector (This.Top) := Item;
      end Push;
      procedure Pop (This : in out Stack_T; Item : out Integer) is
      begin
         Item     := This.Vector (This.Top);
         This.Top := This.Top - 1;
      end Pop;
      function Is_Empty (This : Stack_T) return Boolean is (This.Top = 0);
   end Bounded_Stack;

   with Ada.Text_IO;   use Ada.Text_IO;
   with Bounded_Stack; use Bounded_Stack;
   procedure Main is
      Stack : Stack_T;
      Item  : Integer;
   begin
      Push (Stack, 42);
      Put_Line (Boolean'Image (Is_Empty (Stack)));
      Pop (Stack, Item);
      --Put_Line (Integer'Image (Stack.Top)); -- compile error
      Put_Line (Boolean'Image (Is_Empty (Stack)));
      Put_Line (Item'Image);
   end Main;
