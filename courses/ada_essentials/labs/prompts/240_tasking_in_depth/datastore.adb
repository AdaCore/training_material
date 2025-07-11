package body Datastore is

   -- Create a type to protect individual registers, or a protected
   -- object to protect all registers

   function Read
     (Register : Register_T)
      return Integer is (0);
   procedure Write
     (Register : Register_T;
      Value    : Integer) is null;

end Datastore;
