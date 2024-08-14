========
Lab
========

-----------------------
Tasking In Depth Lab
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
Tasking In Depth Lab Solution - Datastore
--------------------------------------------

.. container:: source_include labs/answers/240_tasking_in_depth.txt :start-after:--Datastore :end-before:--Datastore :code:Ada :number-lines:1

-----------------------------------------------------
Tasking In Depth Lab Solution - Monitor Task Type
-----------------------------------------------------

.. container:: source_include labs/answers/240_tasking_in_depth.txt :start-after:--Task :end-before:--Task :code:Ada :number-lines:1

--------------------------------------
Tasking In Depth Lab Solution - Main
--------------------------------------

.. container:: source_include labs/answers/240_tasking_in_depth.txt :start-after:--Main :end-before:--Main :code:Ada :number-lines:1
