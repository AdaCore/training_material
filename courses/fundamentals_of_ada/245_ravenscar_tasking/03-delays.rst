======
Delays
======

-------------
Delay Keyword
-------------

- :ada:`delay` keyword part of tasking
- Blocks for a time
- Absolute: Blocks until a given :ada:`Ada.Real_Time.Time`
- Relative: exists, but forbidden in Ravenscar

.. code:: Ada

    with Ada.Real_Time; use Ada.Real_Time;

    procedure Main is
        Next : Time := Clock;
    begin
        loop
            Next := Next + Milliseconds (10);
            delay until Next;
        end loop;
    end Main;

