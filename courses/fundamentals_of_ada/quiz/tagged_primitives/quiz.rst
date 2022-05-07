Which declaration(s) will make ``P`` a primitive of ``T1``?

A. | :answermono:`type T1 is tagged null record;`
   | :answermono:`procedure P (O : T1) is null;`
B. | :answermono:`type T0 is tagged null record;`
   | :answermono:`type T1 is new T0 with null record;`
   | :answermono:`type T2 is new T0 with null record;`
   | :answermono:`procedure P (O : T1) is null;`
C. | ``type T1 is tagged null record;``
   | ``generic``
   |    ``type T is tagged private;``
   | ``package G_Pkg is``
   |    ``type T2 is new T with null record;``
   | ``end G_Pkg;``
   | ``package Pkg is new G_Pkg (T1);``
   | ``procedure P (O : T1) is null;``
D. | ``type T1 is tagged null record;``
   | ``generic``
   |    ``type T;``
   | ``procedure G_P (O : T);``
   | ``procedure G_P (O : T) is null;``
   | ``procedure P is new G_P (T1);``
