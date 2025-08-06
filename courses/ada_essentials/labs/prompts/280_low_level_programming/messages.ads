with Crc; use Crc;
package Messages is
   type Message_T is private;
   function Create
     (Text       : String;
      Component3 : Boolean;
      Component4 : Character)
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
      Unique_Id  : Integer;
      Text       : String (1 .. 9);
      Component3 : Boolean;
      Component4 : Character;
      Crc        : Crc_T;
   end record;
end Messages;
