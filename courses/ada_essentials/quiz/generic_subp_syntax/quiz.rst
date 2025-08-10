..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    generic
       type T1 is (<>);
       type T2 (<>) is private;
    procedure Do_Something (A : T1; B : T2);

Which declaration(s) is (are) legal?

A. ``procedure Do_A is new Do_Something (String, String)``
B. ``procedure Do_B is new Do_Something (Character, Character)``
C. ``procedure Do_C is new Do_Something (Integer, Integer)``
D. ``procedure Do_D is new Do_Something (Boolean, Boolean)``

.. container:: animate

    A. T2 can be anything (even unconstrained), but T1 must be discrete
    B. T1 is :ada:`Character` (which is an enumerated type) - discrete
    C. T1 is :ada:`Integer` - discrete
    D. T1 is :ada:`Float` - not discrete
