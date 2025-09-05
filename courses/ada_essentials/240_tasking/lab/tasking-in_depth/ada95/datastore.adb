package body Datastore is
   type Register_Data_T is array (Register_T) of Integer;

   protected Registers is
      function Read (Register : Register_T) return Integer;
      procedure Write (Register : Register_T; Value : Integer);
   private
      Register_Data : Register_Data_T;
   end Registers;

   protected body Registers is
      function Read (Register : Register_T) return Integer is
      begin
         return Register_Data (Register);
      end Read;
      procedure Write (Register : Register_T; Value : Integer) is
      begin
         Register_Data (Register) := Value;
      end Write;
   end Registers;

   function Read (Register : Register_T) return Integer is
   begin
      return Registers.Read (Register);
   end Read;
   procedure Write (Register : Register_T; Value : Integer) is
   begin
      Registers.Write (Register, Value);
   end Write;
end Datastore;
