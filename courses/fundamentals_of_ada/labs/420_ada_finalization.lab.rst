----------------------
Ada.Finalization Lab
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
Ada.Finalization Lab Solution - Spec
----------------------------------------------

.. container:: source_include labs/answers/420_ada_finalization.txt :start-after:--Spec :end-before:--Spec :code:Ada

------------------------------------------------
Ada.Finalization Lab Solution - Implementation
------------------------------------------------

.. container:: source_include labs/answers/420_ada_finalization.txt :start-after:--Body :end-before:--Body :code:Ada

----------------------------------------------
Ada.Finalization Lab Solution - Main
----------------------------------------------

.. container:: source_include labs/answers/420_ada_finalization.txt :start-after:--Main :end-before:--Main :code:Ada
