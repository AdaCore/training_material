========
Lab
========

----------------
Statements Lab
----------------

* Goal

  - Create a simple program to build a sheet for tracking your daily work schedule

* Requirements

  + For each day of the week, print a line for each hour in a working day
  + Working hours are

    + 8 hours on a regular work day (Monday-Friday)
    + 4 hours on Saturday
    + No work on Sunday

  + If there are no hours to print, write a message instead

* Hints

  - Use a :ada:`for` loop to iterate over days of week and hours
  - Use a :ada:`case` statement to determine how many hours in a work day
  - Use an :ada:`if` statement to determine if you are printing a time or message

.. note::

  For simplicity, feel free to use a 24-hour clock

-------------------------
Statements Lab Solution
-------------------------

.. container:: source_include 040_statements/lab/statements/answer/main.adb :code:Ada :number-lines:1
