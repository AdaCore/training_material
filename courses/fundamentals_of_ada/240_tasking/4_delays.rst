======
Delays
======

-------------
Delay keyword
-------------

- :ada:`delay` keyword part of tasking
- Blocks for a time
- Relative: Blocks for at least :ada:`Duration`
- Absolute: Blocks until no earlier than :ada:`Calendar.Time` or :ada:`Real_Time.Time`

.. include:: ../examples/delays/src/main.adb
   :code: Ada
