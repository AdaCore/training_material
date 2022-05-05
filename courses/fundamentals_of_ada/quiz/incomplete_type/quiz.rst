.. code:: Ada

    type T;

Which of the following types is(are) legal?

A. :answermono:`type Acc is access T`
B. ``type Arr is array (1 .. 10) of T``
C. ``type T2 is new T``
D. | :answermono:`type T2 is record`
   |    :answermono:`Acc : access T;`
   | :answermono:`end record;`

.. container:: animate

    D. Be careful about the use of an anonymous type here!
