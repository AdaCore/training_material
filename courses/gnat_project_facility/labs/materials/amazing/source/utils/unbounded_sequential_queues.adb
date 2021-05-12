--
--            Copyright (C) 2008-2010, AdaCore
--

package body Unbounded_Sequential_Queues is

   ------------
   -- Insert --
   ------------

   procedure Insert (Into : in out Queue;  Item : Element) is
      Ptr : Link;
   begin
      Ptr := new Node'(Data => Item, Next => null);
      if Into.Count = 0 then -- initial case
         Into.Rear := Ptr;
         Into.Front := Into.Rear;
         Into.Count := 1;
      else -- nodes already in list
         Into.Rear.Next := Ptr;
         Into.Rear := Ptr;
         Into.Count := Into.Count + 1;
      end if;
   exception
      when Storage_Error =>
         raise Overflow;
   end Insert;

   ------------
   -- Remove --
   ------------

   procedure Remove (From : in out Queue;  Item : out Element) is
   begin
      if From.Count > 0 then -- have data items to Remove
         Item := From.Front.Data;
         From.Front := From.Front.Next;
         From.Count := From.Count - 1;
      else -- user didn't check
         raise Underflow;
      end if;
   end Remove;

   -----------
   -- Empty --
   -----------

   function Empty (Q : Queue) return Boolean is
   begin
      return Q.Count = 0;
   end Empty;

   ----------
   -- Size --
   ----------

   function Size (Q : Queue) return Natural is
   begin
      return Q.Count;
   end Size;

   ---------------
   -- Iteration --
   ---------------

   procedure Iteration (Over : Queue) is
      Ptr     : Link    := Over.Front;
      Enabled : Boolean := True;
   begin
      while Ptr /= null and Enabled loop
         Process (Ptr.Data, Enabled);
         Ptr := Ptr.Next;
      end loop;
   end Iteration;

end Unbounded_Sequential_Queues;
