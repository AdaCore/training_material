========
Lab
========

-----------------------
Program Structure Lab
-----------------------

* Requirements

   - Create a message data type

      + Actual message type should be private
      + Need primitives to construct message and query contents

   - Create a child package that allows clients to modify the contents of the message

   - Main program should

      + Build a message
      + Print the contents of the message
      + Modify part of the message
      + Print the new contents of the message

* **Note: There is no prompt for this lab - you need to learn how to build the program structure**

----------------------------------------------
Program Structure Lab Solution - Messages
----------------------------------------------

.. container:: source_include 130_program_structure/lab/program_structure/answer/messages.ads :code:Ada :number-lines:1

.. container:: source_include 130_program_structure/lab/program_structure/answer/messages.adb :code:Ada :number-lines:1

-------------------------------------------------------
Program Structure Lab Solution - Message Modification
-------------------------------------------------------

.. container:: source_include 130_program_structure/lab/program_structure/answer/messages-modify.ads :code:Ada :number-lines:1

.. container:: source_include 130_program_structure/lab/program_structure/answer/messages-modify.adb :code:Ada :number-lines:1

---------------------------------------
Program Structure Lab Solution - Main
---------------------------------------

.. container:: source_include 130_program_structure/lab/program_structure/answer/main.adb :code:Ada :number-lines:1
