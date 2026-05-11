========
Lab
========

--------------------------
Subprogram Contracts Lab
--------------------------

* Overview

   - Create a priority-based queue ADT

      + Higher priority items come off queue first
      + When priorities are same, process entries in order received
      + Cannot have duplicate entries in the queue

* Requirements

   - Queue type must be private

   - Main program should verify pre-condition failure(s)

      - At least one pre-condition should raise something other than assertion error

   - Post-condition should ensure queue is correctly ordered

* Goals

   - Basically a stack, except insertion doesn't necessarily happen at "top"
   - To enable assertions in the runtime from :toolname:`GNAT Studio`

      * :menu:`Edit` :math:`\rightarrow` :menu:`Project Properties`
      * **Build** :math:`\rightarrow` **Switches** :math:`\rightarrow` **Ada**
      * Click on *Enable assertions*

---------------
Helpful Hints
---------------

* Need contracts to prevent

  * Adding an item to a full queue
  * Removing an item from an empty queue

* Need contracts to ensure

  * Adding item to queue adds one to queue length
  * Removing item from queue subtracts one from queue length

--------------------------------
Extra Credit (If Time Permits)
--------------------------------

* Prevent adding duplicate item to queue

* Do the contracts guarantee

  * Item actually gets added to queue?
  * Adding/removing an item does not otherwise alter the queue?

    * If queue is "A, B, C" and "D" is added, is "D, D, D, D" a valid queue?

------------------------------------
Lab Solution - Queue Spec (Public)
------------------------------------

.. container:: source_include 273_subprogram_contracts/lab/subprogram_contracts/answer/priority_queue.ads :code:Ada :start-after:public_start :end-before:public_end

-------------------------------------
Lab Solution - Queue Spec (Private)
-------------------------------------

.. container:: source_include 273_subprogram_contracts/lab/subprogram_contracts/answer/priority_queue.ads :code:Ada :start-after:private_start :end-before:private_end

-----------------------------
Lab Solution - Queue (Body)
-----------------------------

.. container:: source_include 273_subprogram_contracts/lab/subprogram_contracts/answer/priority_queue.adb :code:Ada :number-lines:1

----------------------------
Lab Solution - Main (Tests)
----------------------------

.. container:: source_include 273_subprogram_contracts/lab/subprogram_contracts/answer/main.adb :code:Ada :start-after:main_part_1_start :end-before:main_part_1_end

-------------------------------
Lab Solution - Main (Support)
-------------------------------

.. container:: source_include 273_subprogram_contracts/lab/subprogram_contracts/answer/main.adb :code:Ada :start-after:main_part_2_start :end-before:main_part_2_end

