.. code:: Ada

    type T1 is tagged null record;
    type T2 is new T1 with null record;
    V : T2;

Which of the following piece(s) of code allow for calling :ada:`Proc (V)`?

A. ``procedure Proc (V : T1) is null``
B. :answermono:`procedure Proc (V : T1'Class) is null`
C. | ``procedure Proc (V : T1'Class) is null;``
   | ``procedure Proc (V : T2'Class) is null;``
D. | :answermono:`procedure Proc (V : T1) is null;`
   | :answermono:`procedure Proc (V : T2) is null;`

.. container:: animate

    A. Proc is not a primitive
    C. :ada:`T1'Class` contains :ada:`T2'Class`
