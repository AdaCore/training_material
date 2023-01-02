..

    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    type T1 is range 0 .. 10;
    type T2 is range 0 .. 10;
    type Tag_T1 is tagged null record;
    type Tag_T2 is tagged null record;

Which of the following piece(s) of code is(are) legal?

A. :answermono:`procedure P (A : T1; B : T2) is null`
B. :answermono:`procedure P (A : T1; B : Tag_T1) is null`
C. :answermono:`procedure P (A : T1; B : Tag_T1; C : Tag_T1) is null`
D. ``procedure P (A : T1; B : Tag_T1; C : Tag_T2) is null``

.. container:: animate

    D. Has two controlling type
