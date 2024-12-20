with Crc; use Crc;
package Messages is
   type Message_T is private;
   function Create
     (Text   : String;
      Field3 : Boolean;
      Field4 : Character)
      return Message_T;
   function Get_Crc
     (Message : Message_T)
      return Crc_T;
   procedure Write (Message : Message_T);
   procedure Read
     (Message : out Message_T;
      Valid   : out Boolean);
   procedure Print (Message : Message_T);
private
   type Message_T is record
      Unique_Id : Integer;
      Text      : String (1 .. 9);
      Field3    : Boolean;
      Field4    : Character;
      Crc       : Crc_T;
   end record;
end Messages;
