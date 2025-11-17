========
Lab
========

----------------
Statements Lab
----------------

* Requirements

  - Create a simple program to build a sheet for tracking your daily work schedule

    + For each day of the week, print a line for the time of day
    + For Monday through Friday, there should be lines for an 8-hour work day
    + For Saturday, there should be lines for a 4-hour work day
    + No work on Sunday, so print a message to that effect


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
