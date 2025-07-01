..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

Which declaration(s) will make ``P`` a primitive of ``T1``?

A. | :answermono:`type T1 is tagged null record;`
   | :answermono:`procedure P (O : T1) is null;`
B. | :answermono:`type T0 is tagged null record;`
   | :answermono:`type T1 is new T0 with null record;`
   | :answermono:`type T2 is new T0 with null record;`
   | :answermono:`procedure P (O : T1) is null;`
C. | ``type T1 is tagged null record;``
   | ``Object : T1;``
   | ``procedure P (O : T1) is null;``
D. | ``package Nested is``
   |    ``type T1 is tagged null record;``
   | ``end Nested;``
   | ``use Nested;``
   | ``procedure P (O : T1) is null;``

.. container:: animate

    A. Primitive (same scope)
    B. Primitive (T1 is not yet frozen)
    C. T1 is frozen by the object declaration
    D. Primitive must be declared in same scope as type
