========
Delays
========

---------------
Delay Keyword
---------------

- :ada:`delay` keyword part of tasking
- Blocks for a time

  - Measured in seconds
  - Resolution dependent on runtime

    - Typically can assume at least 0.1 seconds resolution

- **Relative:** Blocks for at least :ada:`Duration`
- **Absolute:** Blocks until no earlier than :ada:`Calendar.Time` or :ada:`Real_Time.Time`

.. code:: Ada

  with Ada.Calendar; use Ada.Calendar;
  procedure Main is
     Relative : Duration := 1.0;
     Absolute : Time := Time_Of (2027, 12, 31);
  begin
     delay Relative;
     delay until Absolute;
  end Main;
