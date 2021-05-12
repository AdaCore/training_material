--
--            Copyright (C) 2008-2010, AdaCore
--
--
--  This is a thread-safe pool containing access values designating values of
--  an arbitrary type, including indefinite and limited types. The design is
--  that of an "abstract state machine", in that each instance of the generic
--  represents a distinct pool object.

generic
   Pool_Size : Positive;
   type Element (<>) is limited private;
   type Reference is access Element;
package Pool_Manager is

   function Next_Member (Wait : Boolean := True) return Reference;
   --  Returns a member of the pool if one is available. If Wait is true and
   --  the pool is exhausted, waits until a pool member becomes available, in
   --  which case the result will not be null. However, if Wait is False and
   --  the pool is exhausted, returns null.

   procedure Return_Member (This : in out Reference);
   --  Put the item back into the pool.  The value of This is null on return.

   type Initial_Values is array (1 .. Pool_Size) of Reference;

   procedure Load (These : Initial_Values);
   --  Initially put These into the pool.

   Overflow : exception;
   --  raised by Return_Member if insertion of more than Pool_Size references
   --  is attempted

   procedure Await_Quiescence;
   --  await the state in which all members are present

private

   subtype Counter is Integer range 0 .. Pool_Size;
   subtype Index   is Integer range 1 .. Pool_Size;

   type Contents is array (Index) of Reference;

   protected Manager is
      entry Get (R : out Reference);
      procedure Put (R : Reference);
      --  we don't need an entry for Put because this is not a general buffer;
      --  it is a pool of fixed size and content so it should never be full
      --  when a task tries to insert a value, since that value should have
      --  come from the pool in the first place (except for the initial
      --  loading).
      procedure Load (These : Initial_Values);
      entry Await_Quiescence;
   private
      Next_In, Next_Out : Index := 1;
      Pool              : Contents;
      Number_Remaining  : Counter := 0;
   end Manager;

end Pool_Manager;
