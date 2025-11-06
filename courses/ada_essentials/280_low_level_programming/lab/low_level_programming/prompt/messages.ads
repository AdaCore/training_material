with Crc; use Crc;
package Messages is
   type Message_T is private;
   type Kind_T is null record;   -- implement
   type Value_T is null record;  -- implement
   type Text_T is null record;   -- implement
   function Create (Kind  : Kind_T;
                    Value : Value_T;
                    Text  : Text_T)
                    return Message_T;
   procedure Write (Message : Message_T);
   procedure Read (Message : out Message_T; Valid : out Boolean);
   procedure Print (Prompt : String; Message : Message_T);
private
   type Message_T is null record;  -- implement
end Messages;
