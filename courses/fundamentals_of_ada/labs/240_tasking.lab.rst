========
Lab
========

-------------
Tasking Lab
-------------

* Requirements

   - Create multiple tasks with the following attributes

      + Startup entry receives some identifying information and a delay length
      + Stop entry will end the task
      + Until stopped, the task will send it's identifying information to a monitor periodically based on the delay length

   - Create a protected object that stores the identifying information of task that called it

   - Main program should periodically check the protected object, and print when it detects a task switch

      + I.e. If the current task is different than the last printed task, print the identifying information for the current task

-----------------------------------------
Tasking Lab Solution - Protected Object
-----------------------------------------

.. container:: source_include labs/answers/240_tasking.txt :start-after:--Protected :end-before:--Protected :code:Ada :number-lines:1

----------------------------------
Tasking Lab Solution - Task Type
----------------------------------

.. container:: source_include labs/answers/240_tasking.txt :start-after:--Task :end-before:--Task :code:Ada :number-lines:1

----------------------------
Tasking Lab Solution - Main
----------------------------

.. container:: source_include labs/answers/240_tasking.txt :start-after:--Main :end-before:--Main :code:Ada :number-lines:1
