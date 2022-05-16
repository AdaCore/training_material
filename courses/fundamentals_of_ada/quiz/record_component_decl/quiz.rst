.. code:: Ada

    type Record_T is record
       -- Definition here
    end record;

Which record definition is legal?

A. ``Component_1 : array (1 .. 3) of Boolean``
B. :answermono:`Component_2, Component_3 : Integer`
C. ``Component_1 : Record_T``
D. ``Component_1 : constant Integer = 123``

.. container:: animate

    A. Anonymous types not allowed
    B. Correct
    C. No recursive definition
    D. No constant component
