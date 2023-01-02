..

    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    type T1 is array (1 .. 3) of Integer;
    type T2 is array (1 .. 3) of Integer;

Which piece(s) of code correctly perform(s) equality check on A and B?

A. | :answermono:`function "=" (A : T1; B : T2) return Boolean is`
   |   :answermono:`(A = T1 (B));`
B. | ``function "=" (A : T1; B : T2) return Boolean is``
   |   ``(for all E1 of A => (for all E2 of B => E1 = E2));``
C. | ``function "=" (A : T1; B : T2) return Boolean is``
   |   ``(for some E1 of A => (for some E2 of B => A = B));``
D. | :answermono:`function "=" (A : T1; B : T2) return Boolean is`
   |   :answermono:`(for all J in A'Range => A (J) = B (J));`

.. container:: animate

    B. Counterexample: ``A = B = (0, 1, 0)`` returns :ada:`False`
    C. Counterexample: ``A = (0, 0, 1) and B = (0, 1, 1)`` returns :ada:`True`
