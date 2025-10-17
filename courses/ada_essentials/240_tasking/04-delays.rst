======
Delays
======

-------------
Delay Keyword
-------------

- :ada:`delay` keyword part of tasking
- Blocks for a time

  - Measured in seconds
  - Resolution dependent on runtime

    - Typically can assume at least 0.1 seconds resolution

- Relative: Blocks for at least :ada:`Duration`
- Absolute: Blocks until no earlier than :ada:`Calendar.Time` or :ada:`Real_Time.Time`

---------------
Delay Example
---------------

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada

        with Ada.Calendar; use Ada.Calendar;
        with Ada.Text_IO;  use Ada.Text_IO;
        procedure Main is
           Start_Time : Time := Clock;
           function Time_Str return String is
             (Duration'Image (Clock-Start_Time)(1 .. 5));
           task Relative;
           task body Relative is
           begin
              for I in 1 .. 5 loop
                 delay 0.1;
                 Put_Line (Time_Str &
                           " => Relative " & I'Image);
              end loop;
           end Relative;
        begin
           for I in 1 .. 5 loop
              delay until Start_Time + Duration (I) * 0.1;
              Put_Line (Time_Str &
                        " => Absolute " & I'Image);
           end loop;
        end Main;

  .. container:: column

    **Output**

    ``0.10 =>`` :color-red:`Relative  1`

    ``0.10 =>`` :color-green:`Absolute  1`

    ``0.21 =>`` :color-green:`Absolute  2`

    ``0.21 =>`` :color-red:`Relative  2`

    ``0.30 =>`` :color-green:`Absolute  3`

    ``0.33 =>`` :color-red:`Relative  3`

    ``0.41 =>`` :color-green:`Absolute  4`

    ``0.45 =>`` :color-red:`Relative  4`

    ``0.51 =>`` :color-green:`Absolute  5`

    ``0.56 =>`` :color-red:`Relative  5`

