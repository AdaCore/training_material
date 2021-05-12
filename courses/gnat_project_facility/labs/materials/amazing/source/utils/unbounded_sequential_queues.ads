--
--            Copyright (C) 2008-2010, AdaCore
--

--  This package provides queue abstractions that have a capacity limited only
--  by available memory.  These queues are not thread-safe.

generic
   type Element is private;
package Unbounded_Sequential_Queues is

   type Queue is tagged limited private;

   Underflow : exception;
   Overflow  : exception;

   procedure Insert (Into : in out Queue;  Item : Element);
   procedure Remove (From : in out Queue;  Item : out Element);

   function Size (Q : Queue) return Natural;

   function Empty (Q : Queue) return Boolean;
   --  Size(Q) = 0

   generic
      with procedure Process (Item : Element;  Continue : out Boolean);
   procedure Iteration (Over : Queue);

private

   type Node;

   type Link is access Node;

   type Node is
      record
         Data : Element;
         Next : Link;
      end record;

   type Queue is tagged limited
      record
         Count : Natural := 0;
         Rear,
         Front : Link;
      end record;

end Unbounded_Sequential_Queues;
