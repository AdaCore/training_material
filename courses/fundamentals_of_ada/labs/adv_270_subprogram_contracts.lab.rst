--------------------------
Subprogram Contracts Lab
--------------------------

* Overview

   - Create a priority-based queue ADT

      + Higher priority items come off queue first
      + When priorities are same, process entries in order received

* Requirements

   - Main program should verify pre-condition failure(s)

      - At least one pre-condition should raise something other than assertion error

   - Post-condition should ensure queue is correctly ordered

* Hints

   - Basically a stack, except insertion doesn't necessarily happen at "top"
   - To enable assertions in the runtime from :toolname:`GNAT Studio`

      * :menu:`Edit` :math:`\rightarrow` :menu:`Project Properties`
      * **Build** :math:`\rightarrow` **Switches** :math:`\rightarrow` **Ada**
      * Click on *Enable assertions*

--------------------------------------------------
Subprogram Contracts Lab Solution - Queue (Spec)
--------------------------------------------------

.. container:: source_include labs/answers/adv_270_subprogram_contracts.txt :start-after:--Queue_Spec :end-before:--Queue_Spec :code:Ada :number-lines:1

--------------------------------------------------
Subprogram Contracts Lab Solution - Queue (Body)
--------------------------------------------------

.. container:: source_include labs/answers/adv_270_subprogram_contracts.txt :start-after:--Queue_Body :end-before:--Queue_Body :code:Ada :number-lines:1

-------------------------------------------
Subprograms Contracts Lab Solution - Main
-------------------------------------------

.. container:: source_include labs/answers/adv_270_subprogram_contracts.txt :start-after:--Main :end-before:--Main :code:Ada :number-lines:1
