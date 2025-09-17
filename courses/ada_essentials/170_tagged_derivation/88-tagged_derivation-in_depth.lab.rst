========
Lab
========

-----------------------
Tagged Derivation Lab
-----------------------

* Requirements

   - Create a type structure that could be used in a business

      - A **person** has some defining characteristics
      - An **employee** is a *person* with some employment information
      - A **position** is an *employee* with specific job information

   - Create primitive operations to read and print the objects
   - Create a main program to test the objects and operations

* Hints

   - Use `overriding` and `not overriding` as appropriate **(Ada 2005 and above)**
   - Data hiding is important! 

-----------------------------------------------
Tagged Derivation Lab Solution - Types (Spec)
-----------------------------------------------

.. container:: source_include 170_tagged_derivation/lab/tagged_derivation-in_depth/answer/employee.ads :code:Ada :number-lines:1

-------------------------------------------------------
Tagged Derivation Lab Solution - Types (Partial Body)
-------------------------------------------------------

.. container:: source_include 170_tagged_derivation/lab/tagged_derivation-in_depth/answer/employee.adb :start-after:--Types_Body :end-before:--Types_Body :code:Ada :number-lines:1

---------------------------------------
Tagged Derivation Lab Solution - Main
---------------------------------------

.. container:: source_include 170_tagged_derivation/lab/tagged_derivation-in_depth/answer/main.adb :code:Ada :number-lines:1
