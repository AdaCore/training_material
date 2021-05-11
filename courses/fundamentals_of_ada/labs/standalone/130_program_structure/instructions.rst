-----------------------
Program Structure Lab
-----------------------

* Requirements

   - Create a simplistic messaging subsystem

      + Top-level should define a (private) message type and constructor/accessor subprograms
      + Use private child function to calculate message CRC
      + Use child package to add/remove messages to some kind of list

   - Use child package for diagnostics

      + Inject bad CRC into a message
      + Print message contents

   - Main program should

      + Build a list of messages
      + Inject faults into list
      + Print messages in list and indicate if any are faulty

----------------------------------------------
Program Structure Lab Solution - Messages
----------------------------------------------

.. code:: Ada

   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   package Messages is
      type Message_T is private;
      type Kind_T is (Command, Query);
      subtype Content_T is String;

      function Create (Kind    : Kind_T;
                       Content : Content_T)
                       return Message_T;

      function Kind (Message : Message_T) return Kind_T;
      function Content (Message : Message_T) return Content_T;
   private
      type Crc_T is mod Integer'Last;
      type Message_T is record
         Kind    : Kind_T;
         Content : Unbounded_String;
         Crc     : Crc_T;
      end record;
   end Messages;

.. code:: Ada

   with Messages.Crc;
   package body Messages is
      function Create (Kind    : Kind_T;
                       Content : Content_T)
                       return Message_T is
      begin
         return (Kind      => Kind,
                 Content   => To_Unbounded_String (Content),
                 Crc       => Crc (Content));
      end Create;

      function Kind (Message : Message_T) return Kind_T is (Message.Kind);
      function Content (Message : Message_T) return Content_T is (To_String (Message.Content));

end Messages;

------------------------------------------------
Program Structure Lab Solution - Message Queue
------------------------------------------------

.. code:: Ada

   package Messages.Queue is
      function Empty return Boolean;
      function Full return Boolean;
      procedure Push (Message : Message_T);
      procedure Pop (Message : out Message_T;
                     Valid   : out Boolean);
   private
      The_Queue : array (1 .. 10) of Message_T;
      Top       : Integer := 0;
      function Empty return Boolean is (Top = 0);
      function Full return Boolean is (Top = The_Queue'Last);
   end Messages.Queue;

.. code:: Ada

   with Messages.Crc;
   package body Messages.Queue is
      procedure Push (Message : Message_T) is
      begin
         Top             := Top + 1;
         The_Queue (Top) := Message;
      end Push;

      procedure Pop (Message : out Message_T;
                     Valid   : out Boolean) is
      begin
         Message := The_Queue (Top);
         Top     := Top - 1;
         Valid   := Message.Crc = Crc (To_String (Message.Content));
      end Pop;

   end Messages.Queue;

----------------------------------------------
Program Structure Lab Solution - Diagnostics
----------------------------------------------

.. code:: Ada

   package Messages.Queue.Debug is
      function Queue_Length return Integer;
      procedure Inject_Crc_Fault (Position : Integer);
      function Text (Message : Message_T) return String;
   end Messages.Queue.Debug;

.. code:: Ada

   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   package body Messages.Queue.Debug is

      function Queue_Length return Integer is (Top);

      procedure Inject_Crc_Fault (Position : Integer) is
      begin
         Top                 := Position;
         The_Queue (Top).Crc := The_Queue (Top).Crc + 1;
      end Inject_Crc_Fault;

      function Text (Message : Message_T) return String is
        (Kind_T'Image (Message.Kind) & " => " & To_String (Message.Content) &
           " (" & Crc_T'Image (Message.Crc) & " )");

   end Messages.Queue.Debug;

---------------------------------------
Program Structure Lab Solution - Main
---------------------------------------

.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   with Messages;
   with Messages.Queue;
   with Messages.Queue.Debug;
   procedure Main is
      Char    : Character := 'A';
      Content : String (1 .. 10);
      Message : Messages.Message_T;
      Valid   : Boolean;
   begin
      while not Messages.Queue.Full loop
         Content := (others => Char);
         Messages.Queue.Push (Messages.Create (Kind    => Messages.Command,
                                               Content => Content));
         Char := Character'Succ (Char);
      end loop;

      -- inject some faults
      Messages.Queue.Debug.Inject_Crc_Fault (3);
      Messages.Queue.Debug.Inject_Crc_Fault (6);

      while not Messages.Queue.Empty loop
         Put (Integer'Image (Messages.Queue.Debug.Queue_Length) & ") ");
         Messages.Queue.Pop (Message, Valid);
         Put_Line (Boolean'Image (Valid) & " " & Messages.Queue.Debug.Text (Message));
      end loop;

   end Main;

