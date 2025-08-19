package body Messages is

   function Create
     (Text       : String;
      Component3 : Boolean;
      Component4 : Character)
      return Message_T is
   begin
      return
        (Unique_Id => 0, Text => (others => ' '), Component3 => Component3,
         Component4    => Component4, Crc => Crc_T'First);
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
