..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    type T;

Select all types that are legal when defined in the same scope.

A. :answermono:`type Acc is access T`
B. ``type Arr is array (1 .. 10) of T``
C. ``type T2 is new T``
D. | :answermono:`type T2 is record`
   |    :answermono:`Acc : access T;`
   | :answermono:`end record;`

.. container:: animate

    A. Can :ada:`access` the type
    B. Cannot use the type as a component
    C. Cannot derive from an incomplete type
    D. Be careful about the use of an anonymous type here!
