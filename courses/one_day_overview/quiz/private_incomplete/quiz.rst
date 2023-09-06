..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    package Pkg is
       type T is private;
    private
       -- Declarations Here

Which of the following declaration(s) is(are) valid?

A. ``type T is array (Positive range <>) of Integer``
B. :answermono:`type T is tagged null record`
C. ``type T is limited null record``
D. | :answermono:`type T_Arr is array (Positive range <>) of T;`
   | :answermono:`type T is new Integer;`

.. container:: animate

    A. Cannot complete with an unconstrained type
    B. Can complete with the :ada:`tagged` capability
    C. Cannot complete with a :ada:`limited` constraint
    D. Even though T is private, it can be used as component
