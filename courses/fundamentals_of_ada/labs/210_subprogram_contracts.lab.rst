--------------------------
Subprogram Contracts Lab
--------------------------

* Overview

   - Create a priority-based queue ADT

      + Higher priority items come off the queue first
      + When priorities are the same, entries should be processed in order received

* Requirements

   - Main program should verify pre-condition failure(s)

      - At least one pre-condition should raise something other than assertion error

   - Post-condition should ensure queue is correctly ordered

* Hints

   - This is basically a stack, except insertion doesn't necessarily happen at one end

--------------------------------------------------
Subprogram Contracts Lab Solution - Queue (Spec)
--------------------------------------------------

.. container:: source_include labs/answers/210_subprogram_contracts.txt :start-after:--Queue_Spec :end-before:--Queue_Spec :code:Ada

--------------------------------------------------
Subprogram Contracts Lab Solution - Queue (Body)
--------------------------------------------------

.. container:: source_include labs/answers/210_subprogram_contracts.txt :start-after:--Queue_Body :end-before:--Queue_Body :code:Ada

-------------------------------------------
Subprograms Contracts Lab Solution - Main
-------------------------------------------

.. container:: source_include labs/answers/210_subprogram_contracts.txt :start-after:--Main :end-before:--Main :code:Ada
