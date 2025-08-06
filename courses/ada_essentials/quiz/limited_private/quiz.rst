..
    This file is auto-generated from the quiz template, it should not be modified
    directly. Read README.md for more information.

.. code:: Ada

    package P is
       type Priv is private;
    private
       type Lim is limited null record;
       -- Complete Here
    end P;

Which of the following piece(s) of code is (are) legal?

A. | ``type Priv is record``
   |    ``E : Lim;``
   | ``end record;``
B. | :answermono:`type Priv is record`
   |    :answermono:`E : Float;`
   | :answermono:`end record;`
C. | ``type A is array (1 .. 10) of Lim;``
   | ``type Priv is record``
   |    ``F : A;``
   | ``end record;``
D. | :answermono:`type Priv is record`
   |    :answermono:`Component : Integer := Lim'Size;`
   | :answermono:`end record;`

.. container:: animate

    A. ``E`` has limited type, partial view of ``Priv`` must be :ada:`limited private`
    C. ``F`` has limited type, partial view of ``Priv`` must be :ada:`limited private`
