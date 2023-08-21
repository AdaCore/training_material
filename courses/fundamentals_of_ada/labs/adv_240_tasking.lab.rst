-----------------------
Advanced Tasking Lab
-----------------------

* Requirements

   - Create a datastore to set/inspect multiple "registers"

      + Individual registers can be read/written by multiple tasks

   - Create a "monitor" capability that will periodically update each register

      + Each register has it's own update frequency

   - Main program should print register values on request

* Hints

   - Datastore needs to control access to its contents
   - One task per register is easier than one task trying to maintain multiple update frequencies

--------------------------------------------
Advanced Tasking Lab Solution - Datastore
--------------------------------------------

.. container:: source_include labs/answers/adv_240_tasking.txt :start-after:--Datastore :end-before:--Datastore :code:Ada :number-lines:1

-----------------------------------------------------
Advanced Tasking Lab Solution - Monitor Task Type
-----------------------------------------------------

.. container:: source_include labs/answers/adv_240_tasking.txt :start-after:--Task :end-before:--Task :code:Ada :number-lines:1

--------------------------------------
Advanced Tasking Lab Solution - Main
--------------------------------------

.. container:: source_include labs/answers/adv_240_tasking.txt :start-after:--Main :end-before:--Main :code:Ada :number-lines:1
