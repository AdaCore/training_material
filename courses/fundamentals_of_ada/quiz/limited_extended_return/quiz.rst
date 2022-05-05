.. code:: Ada

    type T is limited record
       I : Integer;
    end record;
    O : T := F;

Which declaration(s) of ``F`` is(are) valid?

A. ``function F return T is (I := 1)``
B. :answermono:`function F return T is (I => 1)`
C. ``function F return T is (1)``
D. | :answermono:`function F return T is`
   | :answermono:`begin`
   |    :answermono:`return R : T do`
   |       :answermono:`R.I := 1;`
   |    :answermono:`end return;`
   | :answermono:`end F;`
