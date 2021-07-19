----------------------
Advanced Privacy Lab
----------------------
   
* Requirements
   
   - Create a package defining a message type whose implementation is solely in the body

      - You will need accessor functions to set / get the content
      - Create a function to return a string representation of the message contents

   - Create another package that defines the types needed for a linked list of messages

      - Each message in the list should have an identifier not visible to any clients

   - Create a package containing simple operations on the list

      - Typical operations like list creation and list traversal
      - Create a subprogram to print the list contents
 
   - Have your main program add items to the list and then print the list

* Hints

   - You will need to employ some (but not necessarily all) of the techniques discussed in this module
 
----------------------------------------------
Advanced Privacy Lab Solution - Message Type
----------------------------------------------

.. container:: source_include labs/answers/adv_120_advanced_privacy.txt :start-after:--Messages :end-before:--Messages :code:Ada

---------------------------------------------------
Advanced Privacy Lab Solution - Message List Type
---------------------------------------------------

.. container:: source_include labs/answers/adv_120_advanced_privacy.txt :start-after:--List_Types :end-before:--List_Types :code:Ada
 
---------------------------------------------------------
Advanced Privacy Lab Solution - Message List Operations
---------------------------------------------------------

.. container:: source_include labs/answers/adv_120_advanced_privacy.txt :start-after:--Operations :end-before:--Operations :code:Ada

--------------------------------------
Advanced Privacy Lab Solution - Main
--------------------------------------

.. container:: source_include labs/answers/adv_120_advanced_privacy.txt :start-after:--Main :end-before:--Main :code:Ada

