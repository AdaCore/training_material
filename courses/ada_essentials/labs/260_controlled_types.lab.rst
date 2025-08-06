========
Lab
========

----------------------
Controlled Types Lab
----------------------
   
* Requirements
   
   - Create a simplistic secure key tracker system

      * Keys should be unique
      * Keys cannot be copied
      * When a key is no longer in use, it is returned back to the system

   - Interface should contain the following methods

      * Generate a new key
      * Return a generated key
      * Indicate how many keys are in service
      * Return a string describing the key

   - Create a main program to generate / destroy / print keys
      
* Hints

   - Need to return a key when out-of-scope OR on user request
   - Global data to track used keys
 
----------------------------------------------
Controlled Types Lab Solution - Keys (Spec)
----------------------------------------------

.. container:: source_include labs/answers/260_controlled_types.txt :start-after:--Spec :end-before:--Spec :code:Ada :number-lines:1

------------------------------------------------
Controlled Types Lab Solution - Keys (Body)
------------------------------------------------

.. container:: source_include labs/answers/260_controlled_types.txt :start-after:--Body :end-before:--Body :code:Ada :number-lines:1

----------------------------------------------
Controlled Types Lab Solution - Main
----------------------------------------------

.. container:: source_include labs/answers/260_controlled_types.txt :start-after:--Main :end-before:--Main :code:Ada :number-lines:1
