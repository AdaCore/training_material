--------------------
Type Contracts Lab
--------------------

* Overview

   - Create simplistic class scheduling system

      + Client will specify name, day of week, start time, end time
      + Supplier will add class to schedule
      + Supplier must also be able to print schedule

* Requirements

   - Monday, Wednesday, and/or Friday classes can only be 1 hour long
   - Tuesday and/or Thursday classes can only be 1.5 hours long
   - Classes without a set day meet for any non-negative length of time

* Hints

   - *Subtype Predicate* to create subtypes of day of week
   - *Type Invariant* to ensure that every class meets for correct length of time
   - To enable assertions in the run-time from :toolname:`GNAT Studio`

      * :menu:`Edit` :math:`\rightarrow` :menu:`Project Properties`
      * **Build** :math:`\rightarrow` **Switches** :math:`\rightarrow` **Ada**
      * Click on *Enable assertions*

-----------------------------------------------
Type Contracts Lab Solution - Schedule (Spec)
-----------------------------------------------

.. container:: source_include labs/answers/215_type_contracts.txt :start-after:--Schedule_Spec :end-before:--Schedule_Spec :code:Ada
   
-----------------------------------------------
Type Contracts Lab Solution - Schedule (Body)
-----------------------------------------------

.. container:: source_include labs/answers/215_type_contracts.txt :start-after:--Schedule_Body :end-before:--Schedule_Body :code:Ada
   
------------------------------------
Type Contracts Lab Solution - Main
------------------------------------

.. container:: source_include labs/answers/215_type_contracts.txt :start-after:--Main :end-before:--Main :code:Ada
