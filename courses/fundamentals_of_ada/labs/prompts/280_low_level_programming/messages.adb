package body Messages is

   function Create
     (Text   : String;
      Field3 : Boolean;
      Field4 : Character)
      return Message_T is
   begin
      return
        (Unique_Id => 0, Text => (others => ' '), Field3 => Field3,
         Field4    => Field4, Crc => Crc_T'First);
   end Create;

   -------------
   -- Get_Crc --
   -------------

   function Get_Crc
     (Message : Message_T)
      return Crc_T is
   begin
      return Message.Crc;
   end Get_Crc;

   -----------
   -- Write --
   -----------

   procedure Write (Message : Message_T) is
   begin
      null;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (Message : out Message_T;
      Valid   : out Boolean) is
   begin
      null;
   end Read;

   procedure Print (Message : Message_T) is
   begin
      null;
   end Print;

end Messages;
