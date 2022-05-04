.. code:: Ada

    type My_Array is array (Integer range <>) of Boolean;

How to declare an array of 2 elements?

A. ``O : My_Array (2)``
B. :answermono:`O : My_Array (1 .. 2)`
C. ``O : My_Array (1 .. 3)``
D. ``O : My_Array (1, 2)``

.. container:: animate

    You must declare the :ada:`range` using the :ada:`".."` operator.
    The range :ada:`1 .. 2` has a length of 2.
