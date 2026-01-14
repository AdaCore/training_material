=======
Lab
=======

--------------------------
Advanced Data Hiding Lab
--------------------------
   
* Requirements
   
   - Create a package defining a message type whose implementation is solely in the body

      - You will need accessor functions to set / get the content
      - Create a function to return a string representation of the message contents

   - Create another package that defines the types needed for a linked list of messages

      - Each message in the list should have an identification not visible to any clients

   - Create a package containing simple operations on the list

      - Typical operations like list creation and list traversal
      - Create a subprogram to print the list contents
 
   - Have your main program add items to the list and then print the list

* Hints

   - You will need to employ some (but not necessarily all) of the techniques discussed in this module
 
-----------------------------
Lab Solution - Message Type
-----------------------------

.. container:: source_include 290_advanced_data_hiding/lab/advanced_data_hiding/answer/messages.ads :code:Ada :number-lines:1

----------------------------------
Lab Solution - Message List Type
----------------------------------

.. container:: source_include 290_advanced_data_hiding/lab/advanced_data_hiding/answer/messages-list_types.ads :code:Ada :number-lines:1
 
----------------------------------------
Lab Solution - Message List Operations
----------------------------------------

.. container:: source_include 290_advanced_data_hiding/lab/advanced_data_hiding/answer/messages-list_types-operations.ads :code:Ada :number-lines:1

.. container:: source_include 290_advanced_data_hiding/lab/advanced_data_hiding/answer/messages-list_types-operations.adb :code:Ada :number-lines:1

---------------------
Lab Solution - Main
---------------------

.. container:: source_include 290_advanced_data_hiding/lab/advanced_data_hiding/answer/main.adb :code:Ada :number-lines:1

