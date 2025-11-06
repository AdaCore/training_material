========
Lab
========

-----------------------
Tasking In-Depth Lab
-----------------------

* Requirements

   - Create a datastore to set/inspect multiple "registers"

      + Individual registers can be read/written by multiple tasks

   - Create a "monitor" capability that will periodically update each register

      + Each register has it's own update frequency

   - Main program should initialize the monitors and periodicially print out each register

* Hints

   - Datastore needs to control access to its contents
   - One task per register is easier than one task trying to maintain multiple update frequencies

--------------------------------------------
Tasking In-Depth Lab Solution - Datastore
--------------------------------------------

.. container:: source_include 240_tasking/lab/tasking-in_depth/answer/datastore.ads :code:Ada :number-lines:1

.. container:: source_include 240_tasking/lab/tasking-in_depth/answer/datastore.adb :code:Ada :number-lines:1

-----------------------------------------------------
Tasking In-Depth Lab Solution - Monitor Task Type
-----------------------------------------------------

.. container:: source_include 240_tasking/lab/tasking-in_depth/answer/monitor.ads :code:Ada :number-lines:1

.. container:: source_include 240_tasking/lab/tasking-in_depth/answer/monitor.adb :code:Ada :number-lines:1

--------------------------------------
Tasking In-Depth Lab Solution - Main
--------------------------------------

.. container:: source_include 240_tasking/lab/tasking-in_depth/answer/main.adb :code:Ada :number-lines:1
