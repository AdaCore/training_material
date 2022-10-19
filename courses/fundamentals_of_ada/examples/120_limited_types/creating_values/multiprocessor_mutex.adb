package body Multiprocessor_Mutex is
   procedure Lock (This : in out Also_Limited_T) is null;
   procedure Unlock (This : in out Also_Limited_T) is null;

   Global_Lock : Also_Limited_T := (Lock => (Flag => 0), Id => "GLOB");

   function Create (Flag : Interfaces.Unsigned_8;
                    Id   : Id_T)
                    return Also_Limited_T is
      Local_Lock : Also_Limited_T := (Lock => (Flag => 1), Id => "LOCA");
   begin
      Global_Lock.Lock.Flag := Flag;
      Local_Lock.Id         := Id;

      return (Lock => (Flag => Flag), Id => Id);
   end Create;

end Multiprocessor_Mutex;
