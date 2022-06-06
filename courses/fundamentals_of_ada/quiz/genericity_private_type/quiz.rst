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

Which of the following statement(s) is(are) valid for ``G_P``'s body?

A. :answermono:`pragma Assert (A1 /= null)`
B. ``pragma Assert (A1.all'Size > 32)``
C. :answermono:`pragma Assert (A2 = B2)`
D. ``pragma Assert (A2 - B2 /= 0)``
