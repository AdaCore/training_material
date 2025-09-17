========
Lab
========

--------------------------
Expressions Lab
--------------------------

* Requirements

   - Allow the user to fill a list with dates
   - After the list is created, use *quantified expressions* to print True/False

      * If any date is not legal (taking into account leap years!)
      * If all dates are in the same calendar year

   - Use *expression functions* for all validation routines

* Hints

   - Use subtype membership for range validation
   - You will need *conditional expressions* in your functions
   - You *can* use component-based iterations for some checks

      * But you *must* use indexed-based iterations for others

   - This is the same lab as the *Expressions* lab, we're just replacing
     the validation functions with quantified expressions!

      * So you can just copy that project and update the code!

--------------------------------------------
Expressions Lab Solution - Checks
--------------------------------------------

.. container:: source_include 080_expressions/lab/expressions/answer/main.adb :start-after:--Checks :end-before:--Checks :code:Ada :number-lines:4

------------------------------------------
Expressions Lab Solution - Main
------------------------------------------

.. container:: source_include 080_expressions/lab/expressions/answer/main.adb :start-after:--Main :end-before:--Main :code:Ada :number-lines:37
