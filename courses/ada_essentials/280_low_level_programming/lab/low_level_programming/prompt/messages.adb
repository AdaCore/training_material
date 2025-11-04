package body Messages is

   function Create (Kind  : Kind_T;
                    Value : Value_T;
                    Text  : Text_T)
                    return Message_T is
      Retval : Message_T;
   begin
      return Retval;
  end Create;

   procedure Write (Message : Message_T) is
   begin
      null;
   end Write;

   procedure Read (Message : out Message_T; Valid : out Boolean) is
   begin
      null;
   end Read;

   procedure Print (Prompt : String; Message : Message_T) is
   begin
      null;
   end Print;

end Messages;
