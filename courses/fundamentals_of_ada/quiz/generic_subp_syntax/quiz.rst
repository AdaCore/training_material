..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    generic
       type T1 is (<>);
       type T2 (<>) is private;
    procedure Do_Something (A : T1; B : T2);

Which declaration(s) is(are) legal?

A. ``procedure Do_A is new Do_Something (String, String)``
B. ``procedure Do_B is new Do_Something (Character, Character)``
C. ``procedure Do_C is new Do_Something (Integer, Integer)``
D. ``procedure Do_D is new Do_Something (Boolean, Boolean)``

.. container:: animate

    * :ada:`T2` can be almost anything, so it's not the issue
    * :ada:`T` must be discrete, so it cannot be :ada:`String`
