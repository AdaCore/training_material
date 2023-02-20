..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    type T1 is (NUL, A, B, 'C');
    for T1 use (NUL => 0, A => 1, B => 2, 'C' => 3);
    type T2 is array (Positive range <>) of T1;
    Obj : T2 := "CC" & A & NUL;

Which of the following proposition(s) is(are) true

A. The code fails at runtime
B. ``Obj'Length = 3``
C. :answermono:`Obj (1) = 'C'`
D. :answermono:`Obj (3) = A`
