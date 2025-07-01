..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

       type T is limited record
          I : Integer;
       end record;
    
       function F return T is
       begin
          -- F body...
       end F;
    
       O : T := F;

Which declaration(s) of ``F`` is (are) valid?

A. ``return Return : T := (I => 1)``
B. :answermono:`return Result : T`
C. ``return Value := (others => 1)``
D. | :answermono:`return R : T do`
   |    :answermono:`R.I := 1;`
   | :answermono:`end return;`

.. container:: animate

    A. Using :ada:`return` reserved keyword
    B. OK, default value
    C. Extended return must specify type
    D. OK
