========
Lab
========

---------------------------
Low Level Programming Lab
---------------------------

Message generation / propagation

  * Overview

    * Populate a message structure with data and a CRC (cyclic redundancy check)
    * "Send" and "Receive" messages and verify data is valid

  * Goal

    * You should be able to create, "send", "receive", and print messages
    * Creation should include generation of a CRC to ensure data security
    * Receiving should include validation of CRC

----------------------
Project Requirements
----------------------

* Message Generation

  * Message should at least contain:

    * Unique Identification (integer)
    * Message kind (enumeral)
    * String component of fixed length
    * Floating point value
    * CRC value

  * Note the client should only pass in the kind, string, and float

* "Send" / "Receive"

  * To simulate send/receive:

    * "Send" should do a byte-by-byte write to a text file
    * "Receive" should do a byte-by-byte read from that same text file

  * Receiver should validate received CRC is valid

-------
Hints
-------

* Use a representation clause to specify size of record

  * To get a valid size, individual components may need new types with their own rep spec

* The message kind should define different enumerals to different powers of 2 (e.g. 1, 2, 4, etc)

  * That means only one bit of the value is set

* CRC generation and file read/write should be similar processes

  * Need to convert a message into an array of "something"

-------------
How to Test
-------------

* Main program should generate the message

  * Print the message before sending

* Main program should "send" the message
* Main program then "receives" the message and verifies the CRC
* To simulate failure:

  * Add a delay statement
  * While program is delayed, hand-edit the text file so the data changes
  * Receive the message again

------------------------------------------
Low Level Programming Lab Solution - CRC
------------------------------------------

.. container:: source_include 280_low_level_programming/lab/low_level_programming/answer/crc.ads :code:Ada :number-lines:1

.. container:: source_include 280_low_level_programming/lab/low_level_programming/answer/crc.adb :code:Ada :number-lines:1

------------------------------------------------------
Low Level Programming Lab Solution - Messages (Spec)
------------------------------------------------------

.. container:: source_include 280_low_level_programming/lab/low_level_programming/answer/messages.ads :code:Ada :number-lines:1

-------------------------------------------
Low Level Programming Lab Solution - Main
-------------------------------------------

.. container:: source_include 280_low_level_programming/lab/low_level_programming/answer/main.adb :code:Ada :number-lines:1

---------------------------------------------------------
Low Level Programming Lab Solution - Messages (Helpers)
---------------------------------------------------------

.. container:: source_include 280_low_level_programming/lab/low_level_programming/answer/messages.adb :code:Ada :number-lines:1 :start-after:messages_helpers_begin :end-before:messages_helpers_end

------------------------------------------------------
Low Level Programming Lab Solution - Messages (Body)
------------------------------------------------------

.. container:: source_include 280_low_level_programming/lab/low_level_programming/answer/messages.adb :code:Ada :number-lines:1 :start-after:messages_begin :end-before:messages_end
