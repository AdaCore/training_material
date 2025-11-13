..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    generic
       type T1;
       A1 : access T1;
       type T2 is private;
       A2, B2 : T2;
    procedure G_P;
    procedure G_P is
    begin
       -- Complete here
    end G_P;

Which of the following statement(s) is (are) legal for ``G_P``'s body?

A. :answermono:`if A1 /= null then`
B. ``if A1.all'Size > 32 then``
C. :answermono:`if A2 = B2 then`
D. ``if A2 - B2 /= 0 then``

.. container:: animate

    A. Can always check an access for :ada:`null`
    B. :ada:`T1` is incomplete, so we don't know its size
    C. Comparison of private types is allowed
    D. We do not know if :ada:`T2` allows math
