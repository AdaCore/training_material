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

.. container:: source_include labs/answers/130_program_structure.txt :start-after:--Messages :end-before:--Messages :code:Ada

------------------------------------------------
Program Structure Lab Solution - Message Queue
------------------------------------------------

.. container:: source_include labs/answers/130_program_structure.txt :start-after:--Queue :end-before:--Queue :code:Ada

----------------------------------------------
Program Structure Lab Solution - Diagnostics
----------------------------------------------

.. container:: source_include labs/answers/130_program_structure.txt :start-after:--Debug :end-before:--Debug :code:Ada

----------------------------------------------
Program Structure Lab Solution - CRC
----------------------------------------------

.. container:: source_include labs/answers/130_program_structure.txt :start-after:--CRC :end-before:--CRC :code:Ada

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

