with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Messages is

   type Message_T is private;

   type Kind_T is (Command, Query);

   subtype Content_T is String;

   function Create
     (Kind    : Kind_T;
      Content : Content_T)
      return Message_T;

   function Kind
     (Message : Message_T)
      return Kind_T;
   function Content
     (Message : Message_T)
      return Content_T;

private

   type Crc_T is mod Integer'Last;

   type Message_T is record
      Kind    : Kind_T;
      Content : Unbounded_String;
      Crc     : Crc_T;
   end record;

end Messages;
