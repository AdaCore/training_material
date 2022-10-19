package body Multiprocessor_Mutex is
   procedure Lock (This : in out Also_Limited_T) is null;
   procedure Unlock (This : in out Also_Limited_T) is null;

   Global_Lock_Counter : Interfaces.Unsigned_8 := 0;

   function Create (Id : Id_T) return Also_Limited_T is
   begin
      return Ret_Val : Also_Limited_T do
         if Global_Lock_Counter = Interfaces.Unsigned_8'Last then
            return;
         end if;
         Global_Lock_Counter := Global_Lock_Counter + 1;
         Ret_Val.Id          := Id;
         Ret_Val.Lock.Flag   := Global_Lock_Counter;
      end return;
   end Create;
end Multiprocessor_Mutex;
