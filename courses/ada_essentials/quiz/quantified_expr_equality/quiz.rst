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
   |   ``(for some E1 of A => (for some E2 of B => E1 = E2));``
D. | :answermono:`function "=" (A : T1; B : T2) return Boolean is`
   |   :answermono:`(for all J in A'Range => A (J) = B (J));`

.. container:: animate

    B. Every element of :ada:`A` must match every element of :ada:`B`. This fails when :ada:`A` and :ada:`B` contain more than one distinct value, such as: ``(0, 1, 0)``
    C. Returns :ada:`True` if any value in :ada:`A` matches any value in :ada:`B`, even if the arrays differ elsewhere - ``A = (0, 0, 1) and B = (0, 1, 1)`` returns :ada:`True`
