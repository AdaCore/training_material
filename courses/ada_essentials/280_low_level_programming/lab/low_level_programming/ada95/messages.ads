with Crc; use Crc;
with Interfaces;
package Messages is
   type Message_T is private;
   type Kind_T is (Noop, Direction, Ascend, Descend, Speed);
   for Kind_T use
     (Noop => 0, Direction => 1, Ascend => 2, Descend => 4, Speed => 8);
   for Kind_T'Size use 8;
   subtype Text_T is String (1 .. 7);
   type Value_T is digits 6;
   function Create
     (Kind : Kind_T; Value : Value_T; Text : Text_T) return Message_T;
   procedure Write (Message : Message_T);
   procedure Read (Message : out Message_T; Valid : out Boolean);
   procedure Print (Prompt : String; Message : Message_T);
private
   type Message_T is record
      Unique_Id : Interfaces.Unsigned_32;
      Kind      : Kind_T;
      Value     : Value_T;
      Text      : Text_T;
      Crc       : Crc_T;
   end record;
   for Message_T use
     record
       Unique_Id at 0 range 0 .. 31;
       Kind at 0 range 32 .. 39;
       Value at 0 range 40 .. 71;
       Text at 0 range 72 .. 127;
       Crc at 0 range 128 .. 159;
     end record;
end Messages;
