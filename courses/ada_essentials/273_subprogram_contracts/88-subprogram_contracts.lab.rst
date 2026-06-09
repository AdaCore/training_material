=====
Lab
=====

--------------------------
Subprogram Contracts Lab
--------------------------

* Overview

   - Create a priority-based queue ADT

      + Higher priority items come off queue first
      + When priorities are same, process entries in order received
      + Cannot have duplicate entries in the queue

* Requirements

  - Queue operations always work if called correctly
  - Contracts prevent :ada:`Push` and :ada:`Pop` from being called incorrectly

    - Hint: think about preconditions!

  - Contracts ensure queue is correctly ordered

    - Hint: think about postconditions!

* Goals

   - Basically a stack, except insertion doesn't necessarily happen at "top"

---------------
Helpful Hints
---------------

* Need contracts to prevent

  * Adding an item to a full queue
  * Removing an item from an empty queue

* Need contracts to ensure

  * Adding item to queue adds one to queue length
  * Removing item from queue subtracts one from queue length

* The compiler needs the :command:`-gnata` switch to enable runtime assertions

  * This is added to the :ada:`Compiler` package in the GPR file

    .. code:: Ada

      package Compiler is
        for Switches ("ada") use ("-gnata");
      end Compiler;

.. note::

  The compiler switch is already set in the prompt's :filename:`default.gpr` file

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

