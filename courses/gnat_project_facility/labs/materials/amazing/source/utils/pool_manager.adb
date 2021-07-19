--
--            Copyright (C) 2008-2010, AdaCore
--

package body Pool_Manager is

   -----------------
   -- Next_Member --
   -----------------

   function Next_Member (Wait : Boolean := True) return Reference is
      Ptr : Reference;
   begin
      if Wait then
         Manager.Get (Ptr);
         return Ptr;
      else -- now or never ...
         select
            Manager.Get (Ptr);
            return Ptr;
         else
            return null;
         end select;
      end if;
   end Next_Member;

   -------------------
   -- Return_Member --
   -------------------

   procedure Return_Member (This : in out Reference) is
   begin
      Manager.Put (This);
      This := null;
   end Return_Member;

   ----------
   -- Load --
   ----------

   procedure Load (These : Initial_Values) is
   begin
      Manager.Load (These);
   end Load;

   -------------
   -- Manager --
   -------------

   protected body Manager is

      entry Get (R : out Reference) when Number_Remaining > 0 is
      begin
         R := Pool (Next_Out);
         Next_Out := (Next_Out mod Pool_Size) + 1;
         Number_Remaining := Number_Remaining - 1;
      end Get;

      procedure Put (R : Reference) is
      begin
         if Number_Remaining = Pool_Size then
            raise Overflow;
         end if;
         Pool (Next_In) := R;
         Next_In := (Next_In mod Pool_Size) + 1;
         Number_Remaining := Number_Remaining + 1;
      end Put;

      procedure Load (These : Initial_Values) is
      begin
         for K in These'Range loop
            Pool (Next_In) := These (K);
            Next_In := (Next_In mod Pool_Size) + 1;
            Number_Remaining := Number_Remaining + 1;
         end loop;
      end Load;

      entry Await_Quiescence when Number_Remaining = Pool_Size is
      begin
         null;
      end Await_Quiescence;

   end Manager;

   ----------------------
   -- Await_Quiescence --
   ----------------------

   procedure Await_Quiescence is
   begin
      Manager.Await_Quiescence;
   end Await_Quiescence;

end Pool_Manager;
